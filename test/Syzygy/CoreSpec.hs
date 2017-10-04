module Syzygy.CoreSpec where

import Test.Hspec
import Control.Concurrent
import qualified System.Clock as Clock
import Data.Monoid ((<>))
import Control.Monad (void)
import Data.Function ((&))

import Syzygy.Core
import Syzygy.Signal
import TestUtils (shouldBeLessThan, mean, doUntil)

makeMockBackend :: Chan [(Integer, String)] -> MVar () ->  Backend (CoreConfig String) String
makeMockBackend spyChan sem = MkBackend {toCoreConfig, makeEnv}
  where
    toCoreConfig :: CoreConfig String -> CoreConfig String
    toCoreConfig = id

    sendEvents :: [(Integer, String)] -> IO ()
    sendEvents events = do
      writeChan spyChan events
      takeMVar sem

    makeEnv :: CoreConfig String -> IO (Env String)
    makeEnv _ = return MkEnv { sendEvents }

data MockContext = MkMockContext
  { withMockSendEvent :: forall a. ([ (Integer, String) ] -> IO a) -> IO a
  , getNextNonEmptyBundle :: IO [(Integer, String)]
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal String)
  }

withMockBackend :: forall a. CoreConfig String -> (MockContext -> IO a) -> IO a
withMockBackend MkCoreConfig {bpmRef, signalRef, beatRef} cont = do
  spyChan <- newChan
  (semaphore :: MVar ()) <- newEmptyMVar
  let
    mockBackend :: Backend (CoreConfig String) String
    mockBackend = makeMockBackend spyChan semaphore
  let
    mockConfig :: CoreConfig String
    mockConfig = MkCoreConfig{bpmRef, signalRef, beatRef}
  let
    withMockSendEvent :: forall a. ([(Integer, String)] -> IO a) -> IO a
    withMockSendEvent mockSendEvents = do
      events <- readChan spyChan
      result <- mockSendEvents events
      putMVar semaphore ()
      return result
  let
    getNextNonEmptyBundle :: IO [(Integer, String)]
    getNextNonEmptyBundle = withMockSendEvent return
      & doUntil (\events -> length events > 0)

  threadId <- forkIO $ runBackend mockBackend mockConfig
  result <- cont MkMockContext {withMockSendEvent, getNextNonEmptyBundle, bpmRef, signalRef}
  killThread threadId
  return result

makeDefaultConfig :: IO (CoreConfig String)
makeDefaultConfig = do
  bpmRef <- newMVar 120
  signalRef <- newMVar $ embed "hello"
  beatRef <- newMVar 0
  return MkCoreConfig {bpmRef, signalRef, beatRef}

spec :: Spec
spec = do
  describe "runBackend" $ do
    describe "when invoking sendEvents" $ do
      it "should call sendEvents with the right payloads each frame" $ do
        config <- makeDefaultConfig
        withMockBackend config $ \MkMockContext {getNextNonEmptyBundle} -> do
          let bundlePayloadsShouldBe expectedResult = getNextNonEmptyBundle
                & (fmap . fmap) snd
                & (=<<) (`shouldBe` expectedResult)
          bundlePayloadsShouldBe ["hello"]
          bundlePayloadsShouldBe ["hello"]
          return ()

      it "should call sendEvents with a timestamp delay of less than 2ms" $ do
        config <- makeDefaultConfig
        withMockBackend config $ \MkMockContext {getNextNonEmptyBundle} -> do
          let getLatency = do
                [stamp] <- getNextNonEmptyBundle
                  & (fmap . fmap) fst
                now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
                return (abs (now - stamp))
          getLatency >>= (`shouldBeLessThan` (2 * 10^(9 - 3)))
          getLatency >>= (`shouldBeLessThan` (2 * 10^(9 - 3)))
          getLatency >>= (`shouldBeLessThan` (2 * 10^(9 - 3)))

    describe "timing" $ do
      let
        getTimes :: Int -> Int -> IO [Integer]
        getTimes bpm numBeats = do
          config@MkCoreConfig{bpmRef} <- makeDefaultConfig
          modifyMVar_ bpmRef (const $ return $ bpm)
          withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
            sequence $ replicate (numBeats * 24) $ withMockSendEvent $ \_ -> do
              Clock.toNanoSecs <$> Clock.getTime Clock.Realtime

      describe "clock skew" $ do
        let
          calculateSkew :: Int -> Int -> IO Integer
          calculateSkew bpm numBeats = do
            now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
            times <- getTimes bpm numBeats
            let
              timeElapsed :: Integer
              timeElapsed = head (reverse times) - now
            let
              expectedTimeElapsed :: Integer
              expectedTimeElapsed = (10^9) * fromIntegral numBeats * 60 `div` fromIntegral bpm
            return (expectedTimeElapsed - timeElapsed)

        it "is constant" $ do
          skew <- [1, 2, 4, 8]
            & traverse (calculateSkew 240)
            & (fmap . fmap) fromIntegral
            & (fmap . fmap) (/(10^9))
          let mu = mean skew
          let averageError = mean [(abs (mu - x)) | x <- skew]
          averageError `shouldBeLessThan` 0.001

      describe "clock jitter" $ let
        calculateJitter :: Int -> Int -> IO Double
        calculateJitter bpm numBeats = do
          config@MkCoreConfig{bpmRef} <- makeDefaultConfig
          modifyMVar_ bpmRef (const $ return $ bpm)
          times <- withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
            sequence $ replicate (numBeats * 24) $ withMockSendEvent $ \_ -> do
              Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
          let
            delays :: [Integer]
            delays = tail $ zipWith (-) times (undefined:times)
          let
            expectedDelays :: [Integer]
            expectedDelays = repeat $ 10^9 * 60 `div` fromIntegral bpm `div` fromIntegral _samplesPerBeat
          let
            deltas :: [Double]
            deltas = zipWith (\x y -> (/(10^9)) $ fromIntegral $ abs (x - y)) delays expectedDelays
          return $ mean deltas * fromIntegral _samplesPerBeat

        in void $ ($[1, 2, 4]) $ traverse $ \numBeats -> do
          describe ("over " <> show numBeats <> " beat(s)") $ do
            it "is less than 15ms/beat at 240 bpm" $ do
              let bpm = 240
              jitter <- calculateJitter bpm numBeats
              jitter `shouldBeLessThan` 0.015

            it "is less than 15ms/beat at 120 bpm" $ do
              let bpm = 120
              jitter <- calculateJitter bpm numBeats
              jitter `shouldBeLessThan` 0.015

            it "is less than 15ms/beat at 60bpm" $ do
              let bpm = 60
              jitter <- calculateJitter bpm numBeats
              jitter `shouldBeLessThan` 0.015
