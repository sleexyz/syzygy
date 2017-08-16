{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syzygy where

import Data.Profunctor
import Data.Function ((&))
-- import qualified Network.Socket.ByteString as SB
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Builder as B
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import qualified Data.Time as Time

import qualified Vivid.OSC as OSC

type Time = Rational

type Interval = (Time, Time)

data Event a = MkEvent
  { interval :: (Time, Time)
  , payload :: a
  } deriving (Eq, Show, Functor)

-- | A signal is defined by the "integral" of a sampling function
newtype Signal a = MkSignal { signal :: Interval -> [Event a] }
  deriving (Functor, Monoid)

embed :: a -> Signal a
embed x = MkSignal $ \(queryStart, queryEnd) -> do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return MkEvent { interval = (beat, (beat + 1)), payload = x }

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, queryEnd) ->
  let
    inBounds MkEvent {interval = (start, _)} = start >= queryStart && start < queryEnd
  in
    filter inBounds $ sig (queryStart, queryEnd)

-- | shift forward in time
shift :: Time -> Signal a -> Signal a
shift t MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> (start - t, end - t ))
      & rmap (fmap $ \ev@MkEvent { interval = (start, end) } -> ev { interval = (start + t, end + t) })

-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> ( start * n, end * n ))
      & rmap (fmap $ \ev@MkEvent { interval = (start, end) } -> ev { interval = (start / n, end / n) })

-- | stack in parallel
stack :: [Signal a] -> Signal a
stack sigs = MkSignal $ \query -> do
  MkSignal{signal} <- sigs
  signal query

-- | interleave within one period
interleave :: [Signal a] -> Signal a
interleave sigs = MkSignal $ \query -> do
  let (fromIntegral -> len) = length sigs
  (sig, n) <- zip sigs [0..]
  signal (shift (n/len) sig) query

doOnce :: Rational -> IO () -> IO ()
doOnce actionsPerSecond action = do
  let
    secondsPerAction = recip actionsPerSecond
    picosecondsPerAction = secondsPerAction * 10^6
  threadDelay $ floor picosecondsPerAction
  action

-- | Query a signal for once cycle at the given rate, relative to some absolute time
querySignal :: forall a. Time.UTCTime -> Rational -> Interval -> Signal a -> [(Time.UTCTime, a)]
querySignal now cps query MkSignal{signal} = fmap formatEvent events
  where
    queryStart :: Rational
    (queryStart, _) = query

    events :: [Event a]
    events = signal query

    formatEvent :: Event a -> (Time.UTCTime, a)
    formatEvent MkEvent{interval=(start, _), payload} =
      let
        delay = (start - queryStart)  * (recip cps)
        timestamp = Time.addUTCTime (fromRational delay) now
      in
        (timestamp, payload)

 --   foo :: B.ByteString
 --   foo = OSC.encodeOSC $ OSC.OSC "/play2" [OSC.OSC_S "cps", OSC.OSC_I 1, OSC.OSC_S "s", OSC.OSC_S "bd"]
 -- print $ B.toLazyByteString $ B.byteStringHex foo
