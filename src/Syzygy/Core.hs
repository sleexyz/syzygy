module Syzygy.Core where

import Control.Concurrent
import Control.Monad
import qualified Foreign.Store as ForeignStore

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

_delay :: Int -> Int -> IO ()
_delay bpm ppb = threadDelay ((10^6 * 60) `div` bpm `div` ppb)

runBackend :: Backend config a -> config -> IO ()
runBackend MkBackend {toCoreConfig, makeEnv} config = do
  let MkCoreConfig { bpmRef, signalRef, clockRef } = toCoreConfig config
  let ppb = 1 -- 24 pulses per beat
  MkEnv{sendEvents} <- makeEnv config
  forever $ do
    bpm <- readMVar bpmRef
    sig <- readMVar signalRef
    clockVal <- modifyMVar clockRef (\x -> return (x + (1/fromIntegral ppb), x))
    let events = signal (pruneSignal sig) (clockVal, (1/fromIntegral ppb))
    sendEvents clockVal events
    _delay bpm ppb

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
