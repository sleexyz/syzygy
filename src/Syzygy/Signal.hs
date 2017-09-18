
module Syzygy.Signal where

import Data.Profunctor (lmap, rmap, Profunctor(..))
import Data.Function ((&))

type Interval = (Rational, Rational)

data Event a = MkEvent
  { interval :: (Rational, Rational)
  , payload :: a
  } deriving (Eq, Show, Functor)

mapInterval :: (Interval -> Interval) -> Event a -> Event a
mapInterval f event = event {interval=f (interval event)}

newtype Signal_ i b = MkSignal { signal :: i -> [b] }
  deriving (Functor, Monoid)

instance Profunctor Signal_ where
  lmap (modQuery) sig = MkSignal $ \query -> signal sig $ modQuery query
  rmap (modEvent) sig = MkSignal $ \query -> fmap modEvent $ signal sig query

type Signal a = Signal_ Interval (Event a)

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

slow :: Rational -> Signal a -> Signal a
slow n = fast (1/n)

-- | filter a signal by a predicate on events
_filterSignal :: (Event a -> Bool) -> Signal a -> Signal a
_filterSignal predicate sig = MkSignal $ \query -> filter predicate $ signal sig query

_filterByIndexSieve :: Rational -> Rational -> Signal a -> Signal a
_filterByIndexSieve n i = _filterSignal (makeSieve i)
  where
    makeSieve :: Rational -> Event a -> Bool
    makeSieve i MkEvent { interval = (start, _) } =
      let
        startFract = snd . properFraction @ Rational @ Integer $ start
      in
        startFract >= (i/ n) && startFract < ((i + 1) / n)

fract :: Rational -> Rational
fract x = snd . properFraction @ Rational @ Integer $ x

floor_ :: Rational -> Rational
floor_ = fromIntegral . floor

modR :: Rational -> Rational -> Rational
modR x y = fract (x/y) * y

cat :: [Signal a] -> Signal a
cat sigs = mconcat $ do
  let n = fromIntegral $ length sigs
  (sig, i) <- zip sigs [0..]
  return $ MkSignal $ \(queryStart, queryEnd) -> do
    let f x = floor_ (x/n) + fract x -- TODO: life would be much easier if I could carry over this mapped value, in addition
    let g x = x + i
    let (qs, qe) = (f queryStart, f queryEnd)
    event@MkEvent{interval=(start, _)} <- signal sig (qs, qe)
    event <- event
      & mapInterval (\(s, e) -> (g s, g e))
      & return
    return event


-- | switch between signals within a single cycle
-- TODO: make non lossy
seive :: [Signal a] -> Signal a
seive = makeListCombinator $ \n i sig -> sig
  & _filterByIndexSieve n i

-- | interleave signals within a single cycle
interleave :: [Signal a] -> Signal a
interleave = makeListCombinator $ \n i sig -> sig
  & shift (i/n)
  & _filterByIndexSieve n i

-- | interleaves scaled signals within a single cycle
nest :: [Signal a] -> Signal a
nest = makeListCombinator $ \n i sig -> sig
  & fast n
  & shift (i/n)
  & _filterByIndexSieve n i

makeListCombinator :: (Rational -> Rational -> Signal a -> Signal a) -> [Signal a] -> Signal a
makeListCombinator handler sigs = mconcat $ zipWith (handler n) [0..] sigs
  where
    n :: Rational
    n = fromIntegral $ length sigs
