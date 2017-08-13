{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Syzygy where

import Data.Profunctor
import Data.Function ((&))
import Data.Monoid
import qualified Test.QuickCheck as QC

type Time = Rational

-- | left-closed and right-open intervals
data Interval = MkInterval
  { start :: Time
  , end :: Time
  }
  deriving (Eq, Show)

data Event a = MkEvent
  { query :: Interval
  , payload :: a
  } deriving (Eq, Show, Functor)

data SignalEvent a = MkSignalEvent
  { support :: Interval
  , event :: Event a
  } deriving (Eq, Show, Functor)

type Signal a = Interval -> [SignalEvent a] -- A signal is defined by the "integral" of a sampling function


instance Monoid Interval where
  MkInterval startX endX `mappend` MkInterval startY endY =
    let lengthX = endX - startX
    in MkInterval (startX + lengthX * startY) (startX + lengthX * endY)
  mempty = MkInterval 0 1

instance QC.Arbitrary Interval where
  arbitrary = getInterval <$> QC.arbitrary
    where
      getInterval :: (QC.NonNegative Rational, QC.NonNegative Rational) -> Interval
      getInterval (QC.NonNegative start, QC.NonNegative dur) = MkInterval {start, end = start + dur}

instance Applicative Event where
  pure x = MkEvent { query = MkInterval 0 1, payload = x}

  MkEvent {query = queryF, payload = f} <*> MkEvent {query = queryX, payload = x} =
    MkEvent { query = queryF <> queryX, payload = f x }

instance QC.Arbitrary a => QC.Arbitrary (Event a) where
  arbitrary = do
    query <- QC.arbitrary
    payload <- QC.arbitrary
    return MkEvent {query, payload}

split :: SignalEvent (a -> b) -> SignalEvent a -> [SignalEvent b]
split f x =
  let
    MkSignalEvent { support = supportF , event = MkEvent { query = queryF, payload = payloadF } } = f
    MkSignalEvent { support = supportX, event = MkEvent { query = queryX, payload = payloadX } } = x
  in
    do
      return $ MkSignalEvent { support = supportX, event = MkEvent { query = queryX, payload = payloadF payloadX } }

embed :: a -> Signal a
embed x (MkInterval queryStart queryEnd) = do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return MkSignalEvent { support = MkInterval beat (beat + 1), event = pure x }



prune :: Signal a -> Signal a
prune signal (MkInterval queryStart queryEnd) = filter inBounds $ signal (MkInterval queryStart queryEnd)
  where
    inBounds MkSignalEvent {support = MkInterval{start}} = start >= queryStart && start < queryEnd

-- | shift forward in time
shift :: Time -> Signal a -> Signal a
shift t f = f
  & lmap (\MkInterval{start, end} -> MkInterval { start = start - t, end = end - t })
  & rmap (fmap $ \ev@MkSignalEvent { support = MkInterval start end } -> ev { support = MkInterval (start + t) (end + t) })

stack :: [Signal a] -> Signal a
stack sigs query = do
  sig <- sigs
  sig query

interleave :: [Signal a] -> Signal a
interleave sigs query = do
  let (fromIntegral -> len) = length sigs
  (sig, n) <- zip sigs [0..]
  shift (n/len) sig query

-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n sig = sig
  & lmap (\MkInterval{start, end} -> MkInterval { start = start * n, end = end * n })
  & rmap (fmap $ \ev@MkSignalEvent { support = MkInterval start end } -> ev { support = MkInterval (start / n) (end / n) })

-- ap ::  Signal (a -> b) -> Signal a -> Signal b
-- ap sigF (prune -> sigX) = prune $ \query0 ->  do
--   MkSignalEvent { support = query1, event = f } <- sigF query0
--   MkSignalEvent { support = query2, event = x } <- sigX query1
--   return MkSignalEvent { support = query2, event = f x }

-- A Behavior is a continuous function that is defined at every point in the sampling space
-- type Behavior a = forall b. (Signal (a -> b) -> Signal b)
-- runBehavior :: Behavior (a -> b) -> Signal a -> Signal b
-- runBehavior b s = b $ (fmap . fmap . fmap) (flip ($)) s

-- liftContinuous :: (Time -> a) -> Behavior a
-- liftContinuous fn sig query = do
--   let events = sig query
--   ((s, e), f) <- events
--   let
--     midpoint = (s + e) * 0.5
--     y = fn midpoint
--   return ((s, e), f y)

-- sine :: Behavior Double
-- sine = liftContinuous $ sin . fromRational
