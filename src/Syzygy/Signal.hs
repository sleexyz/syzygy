{-# LANGUAGE MultiWayIf #-}
module Syzygy.Signal where

-- TODO: remove profunctor dependency, use mapQuery, mapEvents instead
import Data.Profunctor (lmap, rmap, Profunctor(..))
import Data.Function ((&))
-- TODO: use mapStart/mapDuration instead of first/second
import Control.Arrow (first, second)

type Interval =
  ( Rational -- start
  , Rational -- duration
  )

data Event_ q a = MkEvent
  { interval :: Interval
  , payload :: a
  } deriving (Eq, Show, Functor)

type Event a = Event_ Interval a

class IntervalMappable t where
  mapInterval :: (Interval -> Interval) -> t a -> t a

instance IntervalMappable (Event_ e) where
  mapInterval f event = event {interval=f (interval event)}

instance IntervalMappable (Signal_ i) where
  mapInterval f sig = MkSignal $ signal sig
    & (fmap . fmap . mapInterval) f

newtype Signal_ i b = MkSignal { signal :: i -> [Event b] }
  deriving (Functor, Monoid)

instance Profunctor Signal_ where
  lmap (modQuery) sig = MkSignal $ \query -> signal sig $ modQuery query
  rmap (modEvent) sig = MkSignal $ \query -> (fmap . fmap) modEvent $ signal sig query

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
  let offset = queryStart - moddedStart `mod_` n
  signal sig (moddedStart, dur)
    & (fmap . mapInterval . first) (+offset)

impulse :: Signal ()
impulse = MkSignal $ splitQueries $ \(queryStart, _) -> if
  | queryStart == 0 -> [MkEvent {interval=(0, 0), payload=()}]
  | otherwise -> []

diracComb :: Signal ()
diracComb = impulse
  & repeatEvery 1
  & (mapInterval . second) (const 1)

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
  & lmap        (\(start, dur) -> (start - t, dur))
  & mapInterval (\(start, dur) -> (start + t, dur))

-- | shift forward in time
-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n sig = sig
  & lmap        (\(start, dur) -> (start * n, dur * n))
  & mapInterval (\(start, dur) -> (start / n, dur/n))

slow :: Rational -> Signal a -> Signal a
slow n = fast (1/n)

-- | Alternate between signals after a cycle
cat :: [Signal a] -> Signal a
cat sigs = mconcat $ do
  let n = fromIntegral $ length sigs
  (sig, i) <- zip sigs [0..]
  return $ MkSignal $ (lmap . first) (subtract i) $ splitQueries $ \(queryStart, dur) -> do
    let moddedStart = floor_ (queryStart/n) + (queryStart) `mod_` n
    let offset = queryStart - moddedStart + i
    event <- signal sig (moddedStart, dur)
    event
      & (mapInterval . first) (+offset)
      & return
      & filter (\MkEvent {interval=(s, _)} -> let pos = s `mod_` n in  pos >= i && pos < (i + 1))

-- | interleave signals within a single cycle
interleave :: [Signal a] -> Signal a
interleave sigs = sigs
  & fmap (slow n)
  & cat
  & fast n
  where
    n = fromIntegral $ length sigs

-- | interleaves scaled signals within a single cycle
nest :: [Signal a] -> Signal a
nest sigs = sigs
  & cat
  & fast n
  where
    n = fromIntegral $ length sigs
