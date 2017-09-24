{-# LANGUAGE MultiWayIf #-}
module Syzygy.Signal where

import Data.Function ((&))
import Data.Profunctor (lmap)
-- TODO: use mapStart/mapDuration instead of first/second

type Interval =
  ( Rational -- start
  , Rational -- duration
  )

data Event_ q a = MkEvent
  { interval :: Interval
  , payload :: a
  } deriving (Eq, Show, Functor)

type Event a = Event_ Interval a

class HasInterval t where
  mapInterval :: (Interval -> Interval) -> t a -> t a

instance HasInterval (Event_ e) where
  mapInterval f event = event {interval=f (interval event)}

instance HasInterval (Signal_ i) where
  mapInterval f sig = MkSignal $ signal sig
    & (fmap . fmap . mapInterval) f

mapStart :: (Rational -> Rational) -> Interval -> Interval
mapStart f (s, d) = (f s, d)

mapDur :: (Rational -> Rational) -> Interval -> Interval
mapDur f (s, d) = (s, f d)

newtype Signal_ i b = MkSignal { signal :: i -> [Event b] }
  deriving (Functor, Monoid)

mapQuery :: (Interval -> Interval) -> Signal a -> Signal a
mapQuery f sig = MkSignal $ \query -> signal sig $ f query

type Signal a = Signal_ Interval a

floor_ :: Rational -> Rational
floor_ = fromIntegral . floor

mod_ :: Rational -> Rational -> Rational
mod_ x y = (snd $ properFraction @ Rational @ Integer $ (x/y)) * y

splitIntervals :: Interval -> [Interval]
splitIntervals (s, d) = let diff = (fromIntegral (ceiling s) - s) in if
  | diff > 0 -> (s, diff) : splitIntervals' (s + diff, d - diff)
  | True -> splitIntervals' (s, d)
  where
  splitIntervals' :: Interval -> [Interval]
  splitIntervals' (s, d) = if
    | d <= 0 -> []
    | d <= 1 -> (s, d) : []
    | d > 1 -> (s, 1) : splitIntervals (s + 1, d - 1)

splitQueries :: (Interval -> [Event a]) -> Interval -> [Event a]
splitQueries sig =  \_rawQuery -> do
  query <- splitIntervals _rawQuery
  sig query

-- lossy
repeatEvery :: Rational -> Signal a -> Signal a
repeatEvery n sig = MkSignal $ splitQueries $ \(queryStart, dur) -> do
  let moddedStart = queryStart `mod_` n
  let offset = queryStart - moddedStart
  signal sig (moddedStart, dur)
    & (fmap . mapInterval . mapStart) (+offset)

impulse :: Signal ()
impulse = MkSignal $ splitQueries $ \(queryStart, _) -> if
  | queryStart == 0 -> [MkEvent {interval=(0, 0), payload=()}]
  | otherwise -> []

diracComb :: Signal ()
diracComb = MkSignal $ splitQueries $ \(queryStart, _) -> if
  | queryStart `mod_` 1 == 0 -> [MkEvent {interval=(queryStart, 1), payload=()}]
  | otherwise -> []

embed :: a -> Signal a
embed x = diracComb
  & fmap (const x)

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, dur) ->
  let
    inBounds MkEvent {interval = (start, _)} = start >= queryStart && start < queryStart + dur
  in
    filter inBounds $ sig (queryStart, dur)

-- | shift forward in time
shift :: Rational -> Signal a -> Signal a
shift t sig = sig
  & mapQuery (\(start, dur) -> (start - t, dur))
  & mapInterval (\(start, dur) -> (start + t, dur))

-- | shift forward in time
-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n sig = sig
  & mapQuery        (\(start, dur) -> (start * n, dur * n))
  & mapInterval (\(start, dur) -> (start / n, dur/n))

slow :: Rational -> Signal a -> Signal a
slow n = fast (1/n)

-- | Alternate between signals after a cycle
cat :: [Signal a] -> Signal a
cat sigs = mconcat $ do
  let n = fromIntegral $ length sigs
  (sig, i) <- zip sigs [0..]
  return $ MkSignal $ (lmap . mapStart) (subtract i) $ splitQueries $ \(queryStart, dur) -> do
    let moddedStart = floor_ (queryStart/n) + (queryStart) `mod_` n
    let offset = queryStart - moddedStart + i
    event <- signal sig (moddedStart, dur)
    event
      & (mapInterval . mapStart) (+offset)
      & return
      & filter (\MkEvent {interval=(s, _)} -> let pos = s `mod_` n in  pos >= i && pos < (i + 1))

-- | interleaves scaled signals within a single cycle
nest :: [Signal a] -> Signal a
nest sigs = sigs
  & cat
  & fast n
  where
    n = fromIntegral $ length sigs


switch :: [Signal a] -> Signal a
switch sigs = mconcat $ do
  let n = fromIntegral $ length sigs
  (sig, i) <- zip sigs [0..]
  return $ MkSignal $ \query-> do
    signal sig query
      & filter (\MkEvent {interval=(s, _)} -> let pos = s `mod_` n in pos >= i && pos < (i + 1))
