
module Syzygy.Signal where

import Data.Profunctor (lmap, rmap)
import Data.Function ((&))

type Interval = (Rational, Rational)

data Event a = MkEvent
  { interval :: (Rational, Rational)
  , payload :: a
  } deriving (Eq, Show, Functor)

newtype Signal a = MkSignal { signal :: Interval -> [Event a] }
  deriving (Functor, Monoid)

embed :: a -> Signal a
embed x = MkSignal $ \(queryStart, queryEnd) -> do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return MkEvent { interval = (beat, beat + 1), payload = x }

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, queryEnd) ->
  let
    inBounds MkEvent {interval = (start, _)} = start >= queryStart && start < queryEnd
  in
    filter inBounds $ sig (queryStart, queryEnd)

-- | shift forward in time
shift :: Rational -> Signal a -> Signal a
shift t MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> (start - t, end - t ))
      & (rmap . fmap) (\ev@MkEvent { interval = (start, end) } -> ev { interval = (start + t, end + t) })

-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> ( start * n, end * n ))
      & (rmap . fmap) (\ev@MkEvent { interval = (start, end) } -> ev { interval = (start / n, end / n) })

-- | filter a signal by a predicate on events
_filterSignal :: (Event a -> Bool) -> Signal a -> Signal a
_filterSignal predicate sig = MkSignal $ \query -> filter predicate $ signal sig query

-- | interleave signals within a single cycle
interleave :: [Signal a] -> Signal a
interleave sigs = mconcat $ filterAndShift <$> zip sigs [0..]
  where
    n :: Rational
    n = fromIntegral $ length sigs

    makeSieve :: Rational -> Event a -> Bool
    makeSieve i MkEvent { interval = (start, _) } =
      let
        startFract = snd $ properFraction @ Rational @ Integer start
      in
        startFract >= (i/ n) && startFract < ((i + 1) / n)

    filterAndShift:: (Signal a, Rational) -> Signal a
    filterAndShift (sig, i) = sig
      & shift (i/n)
      & _filterSignal (makeSieve i)

-- | interleaves scaled signals within a single cycle
nest :: [Signal a] -> Signal a
nest sigs = interleave $ fast n <$> sigs
  where
    n :: Rational
    n = fromIntegral $ length sigs
