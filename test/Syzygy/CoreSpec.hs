module Syzygy.CoreSpec where

import Test.Hspec
import Control.Concurrent
import qualified System.Clock as Clock
import Data.Monoid ((<>))
import Control.Monad (void)
import Data.Function ((&))

import Syzygy.Core
import Syzygy.Signal
import TestUtils (shouldBeLessThan, mean)

makeMockBackend :: Chan (Rational, Integer, [Event String]) -> MVar () ->  Backend (CoreConfig String) String
makeMockBackend spyChan sem = MkBackend {toCoreConfig, makeEnv}
  where
    toCoreConfig :: CoreConfig String -> CoreConfig String
    toCoreConfig = id

    sendEvents :: Rational -> Integer -> [Event String] -> IO ()
    sendEvents clockVal timeVal events = do
      writeChan spyChan (clockVal, timeVal, events)
      takeMVar sem

    makeEnv :: CoreConfig String -> IO (Env String)
    makeEnv _ = return MkEnv { sendEvents }

data MockContext = MkMockContext
  { withMockSendEvent :: (Rational -> Integer -> [ Event String ] -> IO ()) -> IO ()
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal String)
  }

withMockBackend :: CoreConfig String -> (MockContext -> IO ()) -> IO ()
withMockBackend MkCoreConfig {bpmRef, signalRef, beatRef} cont = do
  (spyChan :: Chan (Rational, Integer, [Event String])) <- newChan
  (semaphore :: MVar ()) <- newEmptyMVar
  let
    mockBackend :: Backend (CoreConfig String) String
    mockBackend = makeMockBackend spyChan semaphore
  let
    mockConfig :: CoreConfig String
    mockConfig = MkCoreConfig{bpmRef, signalRef, beatRef}
  let
    withMockSendEvent :: (Rational -> Integer -> [ Event String] -> IO ()) -> IO ()
    withMockSendEvent mockSendEvents = do
      (clockVal, timeVal, events) <- readChan spyChan
      _ <- mockSendEvents clockVal timeVal events
      putMVar semaphore ()
  threadId <- forkIO $ runBackend mockBackend mockConfig
  cont MkMockContext {withMockSendEvent, bpmRef, signalRef}
  killThread threadId

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
      it "should call sendEvents with the right clockVal each frame" $ do
        config <- makeDefaultConfig
        withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
          withMockSendEvent $ \clockVal _ _ -> clockVal `shouldBe` (0/24)
          sequence_ $ replicate 23 $ withMockSendEvent $ \_ _ _ -> return ()
          withMockSendEvent $ \clockVal _ _ -> clockVal `shouldBe` (24/24)

      it "should call sendEvents with the right events each frame" $ do
        config <- makeDefaultConfig
        withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
          withMockSendEvent $ \_ _ events -> events `shouldBe` [ MkEvent {interval=(0, 1), payload="hello"} ]
          sequence_ $ replicate 23 $ withMockSendEvent $ \_ _ events -> events `shouldBe` []
          withMockSendEvent $ \_ _ events -> events `shouldBe` [ MkEvent {interval=(1, 1), payload="hello"} ]

    describe "timing" $ do
      let
        getTimes :: Int -> Int -> IO [Integer]
        getTimes bpm numBeats = do
          logRef <- newMVar []
          config@MkCoreConfig{bpmRef} <- makeDefaultConfig
          modifyMVar_ bpmRef (const $ return $ bpm)
          withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
            sequence_ $ replicate (numBeats * 24) $ withMockSendEvent $ \_ _ _ -> modifyMVar_ logRef $ \xs -> do
              x <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
              return (x:xs)
          readMVar logRef

      describe "clock skew" $ do
        let
          calculateSkew :: Int -> Int -> IO Integer
          calculateSkew bpm numBeats = do
            now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
            times <- getTimes bpm numBeats
            let
              timeElapsed :: Integer
              timeElapsed = head times - now
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
          logRef <- newMVar []
          config@MkCoreConfig{bpmRef} <- makeDefaultConfig
          modifyMVar_ bpmRef (const $ return $ bpm)
          withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
            sequence_ $ replicate (numBeats * 24) $ withMockSendEvent $ \_ _ _ -> modifyMVar_ logRef $ \xs -> do
              x <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
              return (x:xs)
          times <- readMVar logRef
          let
            delays :: [Integer]
            delays = tail $ zipWith (-) (undefined:times) times
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
