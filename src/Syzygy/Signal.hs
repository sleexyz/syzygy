
module Syzygy.Signal where

import Data.Profunctor (lmap, rmap, Profunctor(..))
import Data.Function ((&))

type Interval =
  ( Rational -- start
  , Rational -- duration
  )

data Event_ q a = MkEvent
  { interval :: q
  , payload :: a
  } deriving (Eq, Show, Functor)

type Event = Event_ Interval

mapInterval :: (Interval -> Interval) -> Event a -> Event a
mapInterval f event = event {interval=f (interval event)}

newtype Signal_ i b = MkSignal { signal :: i -> [Event b] }
  deriving (Functor, Monoid)

instance Profunctor Signal_ where
  lmap (modQuery) sig = MkSignal $ \query -> signal sig $ modQuery query
  rmap (modEvent) sig = MkSignal $ \query -> (fmap . fmap) modEvent $ signal sig query

type Signal a = Signal_ Interval a

embed :: a -> Signal a
embed x = MkSignal $ \(queryStart, dur) -> do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryStart + dur
  beat <- [start..end - 1]
  return MkEvent { interval = (beat, 1), payload = x }

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, dur) ->
  let
    inBounds MkEvent {interval = (start, _)} = start >= queryStart && start < queryStart + dur
  in
    filter inBounds $ sig (queryStart, dur)

-- | shift forward in time
shift :: Rational -> Signal a -> Signal a
shift t MkSignal {signal} = MkSignal $ signal
  & lmap                        (\(start, dur) -> (start - t, dur))
  & (rmap . fmap . mapInterval) (\(start, dur) -> (start + t, dur))

-- | shift forward in time
-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n MkSignal {signal} = MkSignal $ signal
  & lmap                        (\(start, dur) -> (start * n, dur * n))
  & (rmap . fmap . mapInterval) (\(start, dur) -> (start / n, dur/n))

slow :: Rational -> Signal a -> Signal a
slow n = fast (1/n)

-- -- | filter a signal by a predicate on events
-- _filterSignal :: (Event a -> Bool) -> Signal a -> Signal a
-- _filterSignal predicate sig = MkSignal $ \query -> filter predicate $ signal sig query

-- _filterByIndexSieve :: Rational -> Rational -> Signal a -> Signal a
-- _filterByIndexSieve n i = _filterSignal (makeSieve i)
--   where
--     makeSieve :: Rational -> Event a -> Bool
--     makeSieve i MkEvent { interval = (start, _) } =
--       let
--         startFract = snd . properFraction @ Rational @ Integer $ start
--       in
--         startFract >= (i/ n) && startFract < ((i + 1) / n)

-- fract :: Rational -> Rational
-- fract x = snd . properFraction @ Rational @ Integer $ x

-- floor_ :: Rational -> Rational
-- floor_ = fromIntegral . floor

-- mod_ :: Rational -> Rational -> Rational
-- mod_ x y = (snd $ properFraction @ Rational @ Integer $ (x/y)) * y

-- makeSieve' :: Rational -> Rational -> Event a -> Bool
-- makeSieve' n i MkEvent { interval = (start, _) } =
--   let
--     moddedStart = start `mod_` n
--   in
--     moddedStart >= i && moddedStart < (i + 1)

-- cat :: [Signal a] -> Signal a
-- cat sigs = pruneSignal $ mconcat $ do
--   let n = fromIntegral $ length sigs
--   (sig, i) <- zip sigs [0..]
--   return $ MkSignal $ \(queryStart, queryEnd) -> do
--     -- let f x = floor_ ((x - i)/n) + (x - i) `mod_` n
--     let f x = x/n
--     let g x = (x) - floor_ (x)
--     let (qs, qe) = (f queryStart, f queryEnd)
--     event@MkEvent{interval=(start, _)} <- signal sig (qs, qe)
--     event
--       & mapInterval (\(s, e) -> (g s, g e))
--       & return
      -- & filter (makeSieve' n i)


-- -- | switch between signals within a single cycle
-- -- TODO: make non lossy
seive :: [Signal a] -> Signal a
seive = undefined
-- seive = makeListCombinator $ \n i sig -> sig
--   & _filterByIndexSieve n i

-- -- | interleave signals within a single cycle
interleave :: [Signal a] -> Signal a
interleave = undefined
-- interleave = makeListCombinator $ \n i sig -> sig
--   & shift (i/n)
--   & _filterByIndexSieve n i

-- -- | interleaves scaled signals within a single cycle
nest :: [Signal a] -> Signal a
nest = undefined
-- nest = makeListCombinator $ \n i sig -> sig
--   & fast n
--   & shift (i/n)
--   & _filterByIndexSieve n i

-- makeListCombinator :: (Rational -> Rational -> Signal a -> Signal a) -> [Signal a] -> Signal a
-- makeListCombinator handler sigs = mconcat $ zipWith (handler n) [0..] sigs
--   where
--     n :: Rational
--     n = fromIntegral $ length sigs
