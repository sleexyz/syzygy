module Syzygy.Core where

import Control.Concurrent
import Control.Monad
import qualified Foreign.Store as ForeignStore
import qualified System.Clock as Clock

import Syzygy.Signal

data CoreConfig a = MkCoreConfig
  { bpmRef :: MVar Int
  , signalRef :: MVar (Signal a)
  , clockRef :: MVar Rational
  -- , latencyNs :: Integer
  }

data Backend config a = MkBackend
  { toCoreConfig :: config -> CoreConfig a
  , makeEnv :: config -> IO (Env a)
  }

newtype Env a = MkEnv
  { sendEvents :: Rational -> Integer -> [Event a] -> IO ()
  }

runBackend :: Backend config a -> config -> IO ()
runBackend MkBackend {toCoreConfig, makeEnv} config = do
  let MkCoreConfig { bpmRef, signalRef, clockRef } = toCoreConfig config
  let ppb = 24 -- 24 pulses per beat
  MkEnv{sendEvents} <- makeEnv config
  lastTimeRef <-  newMVar =<< Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
  forever $ do
    bpm <- readMVar bpmRef
    sig <- readMVar signalRef
    let
      offsetClock :: Rational
      offsetClock = 1 / fromIntegral ppb

      offsetTime :: Integer
      offsetTime = ((10^9 * 60) `div` fromIntegral bpm `div` fromIntegral ppb)

    clockVal <- modifyMVar clockRef (\x -> return (x + offsetClock, x))
    lastTime <- modifyMVar lastTimeRef (\x -> return (x + offsetTime, x))

    let events = signal (pruneSignal sig) (clockVal, offsetClock)
    sendEvents clockVal lastTime events

    let expectedTimeNextTick = lastTime + offsetTime

    now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime

    let timeToWait = (expectedTimeNextTick - now) * 8 `div` 10
    threadDelay (fromIntegral $ (timeToWait `div` 1000))

runOnce :: IO a -> IO a
runOnce computation = do
  let index = 0
  result <- ForeignStore.lookupStore index
  case result of
    Just _ -> ForeignStore.readStore (ForeignStore.Store index)
    Nothing -> do
      x <- computation
      ForeignStore.writeStore (ForeignStore.Store index) x
      return x
