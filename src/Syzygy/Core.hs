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
  { sendEvents :: Rational -> [Event a] -> IO ()
  }

runBackend :: Backend config a -> config -> IO ()
runBackend MkBackend {toCoreConfig, makeEnv} config = do
  let MkCoreConfig { bpmRef, signalRef, clockRef } = toCoreConfig config
  let ppb = 1 -- 24 pulses per beat
  MkEnv{sendEvents} <- makeEnv config
  lastTimeRef <-  newMVar =<< Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
  forever $ do
    lastTime <- readMVar lastTimeRef
    bpm <- readMVar bpmRef
    sig <- readMVar signalRef
    clockVal <- modifyMVar clockRef (\x -> return (x + (1/fromIntegral ppb), x))
    let events = signal (pruneSignal sig) (clockVal, (1/fromIntegral ppb))
    sendEvents clockVal events

    let expectedOffset = ((10^9 * 60) `div` fromIntegral bpm `div` fromIntegral ppb)
    let expectedTimeNow = lastTime + expectedOffset
    actualTimeNow <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
    let delta = actualTimeNow - expectedTimeNow
    modifyMVar_ lastTimeRef(const . return $ expectedTimeNow)
    threadDelay (fromIntegral $ (expectedOffset - delta) `div` 1000)

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
