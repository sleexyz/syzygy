module Syzygy.CoreSpec where

import Test.Hspec
import Control.Concurrent
import qualified Data.Time as Time

import Syzygy.Core
import Syzygy.Signal
import TestUtils (shouldBeLessThan)

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
withMockBackend MkCoreConfig {bpmRef, signalRef, clockRef} cont = do
  (spyChan :: Chan (Rational, Integer, [Event String])) <- newChan
  (semaphore :: MVar ()) <- newEmptyMVar
  let
    mockBackend :: Backend (CoreConfig String) String
    mockBackend = makeMockBackend spyChan semaphore

    mockConfig :: CoreConfig String
    mockConfig = MkCoreConfig{bpmRef, signalRef, clockRef}

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
  clockRef <- newMVar 0
  return MkCoreConfig {bpmRef, signalRef, clockRef}

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
        getDeltas :: Int -> Int -> IO [Time.NominalDiffTime]
        getDeltas bpm numBeats = do
          logRef <- newMVar []
          config@MkCoreConfig{bpmRef} <- makeDefaultConfig
          modifyMVar_ bpmRef (const $ return $ bpm)
          withMockBackend config $ \MkMockContext {withMockSendEvent} -> do
            sequence_ $ replicate (numBeats * 24) $ withMockSendEvent $ \_ _ _ -> modifyMVar_ logRef $ \xs -> do
              x <- Time.getCurrentTime
              return (x:xs)
          times <- readMVar logRef
          let
            delays :: [Time.NominalDiffTime]
            delays = tail $ zipWith (flip Time.diffUTCTime) times (undefined:times)

            expectedDelays :: [Time.NominalDiffTime]
            expectedDelays = repeat $ (1 * (60/fromIntegral bpm) / 24)

            deltas :: [Time.NominalDiffTime]
            deltas = zipWith (\x y -> abs (x - y)) delays expectedDelays
          return deltas

      describe "over 24 samples" $ do
        it "has a net shift of less than 10ms, at 240 bpm" $ do
          let bpm = 240
          deltas <- getDeltas bpm 1
          sum deltas `shouldBeLessThan` 0.010

        it "has a net shift of less than 15ms, at 120 bpm" $ do
          let bpm = 120
          deltas <- getDeltas bpm 1
          sum deltas `shouldBeLessThan` 0.015

        it "has a net shift of less than 15ms at 60bpm" $ do
          let bpm = 60
          deltas <- getDeltas bpm 1
          sum deltas `shouldBeLessThan` 0.015
