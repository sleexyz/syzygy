module Syzygy.Core where

import Data.Profunctor (lmap)
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

type SimpleBackend a = [(Integer, a)] -> IO ()

type Backend a = Int -> (Rational, Rational) -> Integer -> Signal a -> IO ()

fromSimpleBackend :: forall a. SimpleBackend a -> Backend a
fromSimpleBackend sendTimestampedEvents bpm (beat, beatOffset) clock sig = sendTimestampedEvents timestampedEvents
  where
    timestampedEvents :: [(Integer, a)]
    timestampedEvents = signal sig (beat, beatOffset)
        & fmap (makeTimestamp bpm beat clock)


_samplesPerBeat :: Num a => a
_samplesPerBeat = 24

runBackend :: forall a. Backend a -> CoreConfig a -> IO ()
runBackend backend MkCoreConfig{bpmRef, signalRef, beatRef} = do
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
    backend bpm (beat, beatOffset) clock sig
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

extendBackend :: forall a b. (a -> b) -> SimpleBackend b -> SimpleBackend a
extendBackend = (lmap . fmap . fmap)

distribute :: forall a b x. [(x, Either a b)] -> ([(x, a)], [(x, b)])
distribute =
  let
    f (i, Left l) (ls, rs) = ((i,l):ls, rs)
    f (i, Right r) (ls, rs) = (ls, (i,r):rs)
  in
    foldr f ([], [])

combineBackends :: forall a b. SimpleBackend a -> SimpleBackend b -> SimpleBackend (Either a b)
combineBackends sendEventsA  sendEventsB events = do
  let (as, bs) = distribute events
  sendEventsA as
  sendEventsB bs

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
