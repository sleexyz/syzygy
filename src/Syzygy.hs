{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Syzygy where

import Data.Function ((&))
import Data.Profunctor

type Time = Rational
type Interval = (Time, Time) -- left-closed and right-open intervals
type Event a = (Interval, a)
type Signal a = Interval -> [Event a] -- A signal is defined by the "integral" of a sampling function

-- newtype Behavior a = MkBehavior (forall b. (Signal (a -> b) -> Signal b)) -- TODO: checkout paf31's encoding, looks like a constrained signal transformer

embed :: a -> Signal a
embed x (queryStart, queryEnd) = do
  let
    start = (fromIntegral @Integer) . ceiling $ queryStart
    end = (fromIntegral @Integer) . floor $ queryEnd - 1/256 -- FIMXE: make less hacky
  beat <- [start..end]
  return ((beat, beat + 1), x)

fast :: Rational -> Signal a -> Signal a
fast n = dimap mapQuery mapResult
  where
    mapQuery :: Interval -> Interval
    mapQuery (start, end) = (start * n, end * n)

    mapResult :: [Event a] -> [Event a]
    mapResult res = do
      ((start, end), x) <- res
      return $ ((start / n, end / n), x)

showSignal :: (Show a) => Signal a -> String
showSignal pat = pat & ($(0, 1)) & show
