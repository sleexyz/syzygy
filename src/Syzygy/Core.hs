module Syzygy.Core where

import Control.Monad
import Control.Concurrent
import Syzygy hiding (Config(..), Env(..), makeEnv)

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
