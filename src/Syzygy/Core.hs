module Syzygy.Core where

import Control.Concurrent
import Control.Monad

import Syzygy.Signal

data CoreConfig a = MkCoreConfig
  { bpmRef :: MVar Int
  , signalRef :: MVar (Signal a)
  , clockRef :: MVar Rational
  }

data Backend config a = MkBackend
  { toCoreConfig :: config -> CoreConfig a
  , makeEnv :: config -> IO (Env a)
  }

newtype Env a = MkEnv
  { sendEvents :: Rational -> [Event a] -> IO ()
  }

delayOneBeat :: Int -> Int -> IO ()
delayOneBeat bpm spb = threadDelay ((10^6 * 60) `div` bpm `div` spb)

runBackend :: Backend config a -> config -> IO ()
runBackend MkBackend {toCoreConfig, makeEnv} config = do
  let MkCoreConfig { bpmRef, signalRef, clockRef } = toCoreConfig config
  let tpb = 24 -- 24 pulses per quarter note
  MkEnv{sendEvents} <- makeEnv config
  forever $ do
    bpm <- readMVar bpmRef
    sig <- readMVar signalRef
    clockVal <- modifyMVar clockRef (\x -> return (x + (1/fromIntegral tpb), x))
    let events = signal (pruneSignal sig) (clockVal, clockVal + (1/fromIntegral tpb))
    sendEvents clockVal events
    delayOneBeat bpm tpb
