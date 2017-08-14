{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Syzygy where

import Data.Profunctor
import Data.Function ((&))
import Data.Monoid

type Time = Rational

type Interval = (Time, Time)

data Event a = MkEvent
  { query :: (Time, Time)
  , payload :: a
  } deriving (Eq, Show, Functor)

newtype Signal a = MkSignal { signal :: Interval -> [Event a] } -- A signal is defined by the "integral" of a sampling function


combineEvent :: forall a. Monoid a => Event a -> Event a -> [Event a]
combineEvent x y = headX <> headY <> overlap <> tailY <> tailX
  where
    MkEvent { query = (startX, endX), payload = payloadX } = x
    MkEvent { query = (startY, endY), payload = payloadY } = y
    overlap = if start >= end then [] else return $ MkEvent { query = (start, end), payload = payloadX <> payloadY }
      where
        start = max startX startY
        end = min endX endY

    tailY = if start >= end then [] else return $ MkEvent { query = (start, end), payload = payloadY }
      where
        start = max startY endX
        end = endY

    tailX = if start >= end then [] else return $ MkEvent { query = (start, end), payload = payloadX }
      where
        start = max startX endY
        end = endX

    headX = if start >= end then [] else return $ MkEvent { query = (start, end), payload = payloadX }
      where
        start = startX
        end = min endX startY

    headY = if start >= end then [] else return $ MkEvent { query = (start, end), payload = payloadY }
      where
        start = startY
        end = min startX endY

combineEventOverlap :: forall a. Monoid a => Event a -> Event a -> [Event a]
combineEventOverlap x y = overlap
  where
    MkEvent { query = (startX, endX), payload = payloadX } = x
    MkEvent { query = (startY, endY), payload = payloadY } = y
    overlap = if start >= end then [] else return $ MkEvent { query = (start, end), payload = payloadX <> payloadY }
      where
        start = max startX startY
        end = min endX endY

embed :: a -> Signal a
embed x = MkSignal $ \(queryStart, queryEnd) -> do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return MkEvent { query = (beat, (beat + 1)), payload = x }

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, queryEnd) ->
  let
    inBounds MkEvent {query = (start, _)} = start >= queryStart && start < queryEnd
  in
    filter inBounds $ sig (queryStart, queryEnd)

instance Monoid a => Monoid (Signal a) where
  mempty = MkSignal $ \_ -> []

  (MkSignal sigX) `mappend` (MkSignal sigY) = MkSignal $ \query ->
    let
      xs = sigX query
    in
      case xs of
        [] -> sigY query
        _ ->
          let
            f x@MkEvent{query=subQuery} acc = case sigY subQuery of
              [] -> (pure x <> acc)
              ys -> (<> acc) $ do
                y <- ys
                x `combineEventOverlap` y
          in
            foldr f [] xs


-- -- | shift forward in time
-- shift :: Time -> Signal a -> Signal a
-- shift t f = f
--   & lmap (\MkInterval{start, end} -> MkInterval { start = start - t, end = end - t })
--   & rmap (fmap $ \ev@MkSignalEvent { support = MkInterval start end } -> ev { support = MkInterval (start + t) (end + t) })

-- stack :: [Signal a] -> Signal a
-- stack sigs query = do
--   sig <- sigs
--   sig query

-- interleave :: [Signal a] -> Signal a
-- interleave sigs query = do
--   let (fromIntegral -> len) = length sigs
--   (sig, n) <- zip sigs [0..]
--   shift (n/len) sig query

-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> ( start * n, end * n ))
      & rmap (fmap $ \ev@MkEvent { query = (start, end) } -> ev { query = (start / n, end / n) })

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
