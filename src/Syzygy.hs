{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Syzygy where

import Data.Profunctor
import Data.Function ((&))

type Time = Rational
type Interval = (Time, Time) -- left-closed and right-open intervals
type Event a = (Interval, a)
type Signal a = Interval -> [Event a] -- A signal is defined by the "integral" of a sampling function

embed :: a -> Signal a
embed x (queryStart, queryEnd) = do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return ((beat, beat + 1), x)


prune :: Signal a -> Signal a
prune signal (queryStart, queryEnd) = filter inBounds $ signal (queryStart, queryEnd)
  where
    inBounds ((s, _), _) = s >= queryStart && s < queryEnd

-- | shift forward in time
shift :: Time -> Signal a -> Signal a
shift t f = f
  & lmap (\(start, end) -> (start - t, end - t))
  & rmap (\res -> [((start + t, end + t), x) | ((start, end), x) <- res])

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
  & lmap (\(start, end) -> (start * n, end * n))
  & rmap (\res -> [((start/n, end/n), x) | ((start, end), x) <- res])

ap ::  Signal (a -> b) -> Signal a -> Signal b
ap sigF (prune -> sigX) = prune $ \query0 ->  do
  (query1, f) <- sigF query0
  (query2, x) <- sigX query1
  return (query2, f x)

-- A Behavior is a continuous function that is defined at every point in the sampling space
type Behavior a = forall b. (Signal (a -> b) -> Signal b)
runBehavior :: Behavior (a -> b) -> Signal a -> Signal b
runBehavior b s = b $ (fmap . fmap . fmap) (flip ($)) s

liftContinuous :: (Time -> a) -> Behavior a
liftContinuous fn sig query = do
  let events = sig query
  ((s, e), f) <- events
  let
    midpoint = (s + e) * 0.5
    y = fn midpoint
  return ((s, e), f y)

sine :: Behavior Double
sine = liftContinuous $ sin . fromRational
