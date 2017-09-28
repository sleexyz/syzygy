module Syzygy.Core where

import Control.Concurrent
import Control.Monad
import qualified Foreign.Store as ForeignStore
import qualified System.Clock as Clock

import Syzygy.Signal

data CoreConfig a = MkCoreConfig
  { bpmRef :: MVar Int
  , signalRef :: MVar (Signal a)
  , beatRef :: MVar Rational
  }

data Backend config a = MkBackend
  { toCoreConfig :: config -> CoreConfig a
  , makeEnv :: config -> IO (Env a)
  }

newtype Env a = MkEnv
  { sendEvents :: Rational -> Integer -> [Event a] -> IO ()
  }

_samplesPerBeat :: Num a => a
_samplesPerBeat = 24

runBackend :: Backend config a -> config -> IO ()
runBackend MkBackend {toCoreConfig, makeEnv} config = do
  let MkCoreConfig {bpmRef, signalRef, beatRef} = toCoreConfig config
  MkEnv{sendEvents} <- makeEnv config
  clockRef <-  newMVar =<< Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
  forever $ do
    bpm <- readMVar bpmRef
    sig <- readMVar signalRef
    let
      beatOffset :: Rational
      beatOffset = 1 / _samplesPerBeat
    let
      clockOffset :: Integer
      clockOffset = ((10^9 * 60) `div` fromIntegral bpm `div` _samplesPerBeat)
    beat <- modifyMVar beatRef (\beat -> return (beat + beatOffset, beat))
    clock <- modifyMVar clockRef (\clock -> return (clock + clockOffset, clock))
    sendEvents beat clock $ signal (pruneSignal sig) (beat, beatOffset)
    waitTil (clock + clockOffset)

-- | waits until t nanoseconds since UNIX epoch
waitTil :: Integer -> IO ()
waitTil t = do
  now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
  let timeToWait = t - now
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
