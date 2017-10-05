module Syzygy.Core where

import Data.Function ((&))
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
  { makeEnv :: config -> IO (Env a)
  }

newtype Env a = MkEnv
  { sendEvents :: [(Integer, a)] -> IO ()
  }

_samplesPerBeat :: Num a => a
_samplesPerBeat = 24

runBackend :: forall a config. Backend config a -> config -> CoreConfig a -> IO ()
runBackend MkBackend {makeEnv} config MkCoreConfig{bpmRef, signalRef, beatRef} = do
  MkEnv{sendEvents} <- makeEnv config
  clockRef <- newMVar =<< Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
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
    let
      timestampedEvents :: [(Integer, a)]
      timestampedEvents = signal sig (beat, beatOffset)
          & fmap (makeTimestamp bpm beat clock)
    sendEvents timestampedEvents
    waitTil (clock + clockOffset)

makeTimestamp :: Int -> Rational -> Integer -> Event a -> (Integer, a)
makeTimestamp bpm beatStart clock MkEvent{interval=(eventStart, _), payload} = (clock + offset, payload)
  where
    offset :: Integer
    offset = floor $ (10^9 * 60) * (eventStart - beatStart) / fromIntegral bpm

-- | waits until t nanoseconds since UNIX epoch
waitTil :: Integer -> IO ()
waitTil t = do
  now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
  let timeToWait = t - now
  threadDelay (fromIntegral $ (timeToWait `div` 1000))

extendBackend :: forall a b config. (a -> b) -> Backend config b -> Backend config a
extendBackend f MkBackend{makeEnv} = MkBackend {makeEnv = newMakeEnv}
  where
    newMakeEnv :: config -> IO (Env a)
    newMakeEnv c = do
      MkEnv{sendEvents} <- makeEnv c
      let
        newSendEvents :: [(Integer, a)] -> IO ()
        newSendEvents events = sendEvents ((fmap . fmap) f events)
      return MkEnv {sendEvents=newSendEvents}

distribute :: forall a b x. [(x, Either a b)] -> ([(x, a)], [(x, b)])
distribute =
  let
    f (i, Left l) (ls, rs) = ((i,l):ls, rs)
    f (i, Right r) (ls, rs) = (ls, (i,r):rs)
  in
    foldr f ([], [])

combineBackends :: forall a configA b configB. Backend configA a -> Backend configB b -> Backend (configA, configB) (Either a b)
combineBackends MkBackend{makeEnv=makeEnvA} MkBackend{makeEnv=makeEnvB} = MkBackend {makeEnv}
  where
    makeEnv :: (configA, configB) -> IO (Env (Either a b))
    makeEnv (conA, conB) = do
      MkEnv{sendEvents=sendEventsA} <- makeEnvA conA
      MkEnv{sendEvents=sendEventsB} <- makeEnvB conB
      let
        sendEvents :: [(Integer, Either a b)] -> IO ()
        sendEvents events = do
          let (as, bs) = distribute events
          sendEventsA as
          sendEventsB bs
      return MkEnv {sendEvents}

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
