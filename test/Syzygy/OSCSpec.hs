module Syzygy.OSCSpec where

import Control.Concurrent
import TestUtils (shouldBeLessThan, mean)
import Data.Function ((&))
import Test.Hspec
import Vivid.OSC (OSCBundle(..), decodeOSCBundle, OSCDatum(OSC_S), OSC(..), Timestamp(..), utcToTimestamp)
import qualified Data.Time as Time
import Control.Monad
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import qualified Data.ByteString as BS

import Syzygy.OSC
import Syzygy.Signal
import Syzygy.Core

diffTimestamp :: Timestamp -> Timestamp -> Double
diffTimestamp (Timestamp x) (Timestamp y) = x - y

data TestContext = MkTestContext
  { receiveOSCBundle :: forall a. (OSCBundle -> IO a) -> IO a
  , getMessage :: IO OSC
  , getTimestamp :: IO Timestamp
  }

withMockOSCServer :: (OSCBundle -> IO ()) -> (Network.PortNumber -> IO a)  -> IO a
withMockOSCServer handleOSCBundle continuation = do
  address <- head <$> Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show Network.aNY_PORT))
  socket <- Network.socket (Network.addrFamily address) Network.Datagram Network.defaultProtocol
  Network.bind socket (Network.addrAddress address)
  threadId <- forkIO $ forever $ do
    msg <- NetworkBS.recv socket 4096
    let Right bundle = decodeOSCBundle msg
    handleOSCBundle bundle
  portNumber <- Network.socketPort socket
  result <- continuation portNumber
  killThread threadId
  return result

withMockOSC :: Signal [OSC] -> Int -> (TestContext -> IO a) -> IO a
withMockOSC defaultSignal bpm continuation = do
  (bundleChan :: MVar OSCBundle) <- newEmptyMVar
  withMockOSCServer (putMVar bundleChan) $ \portNumber -> do
    bpmRef <- newMVar bpm
    signalRef <- newMVar defaultSignal
    beatRef <- newMVar 0
    let coreConfig = MkCoreConfig{bpmRef, signalRef, beatRef}
    backend <- makeOSCSendTimestampedEvents MkOSCConfig { portNumber }
    clientThread <- forkIO $ runBackend (fromSendTimestampedEvents backend) coreConfig
    let receiveOSCBundle bundleHandler = do
          bundleVal <- takeMVar bundleChan
          bundleHandler bundleVal
    let getMessage = receiveOSCBundle $ \(OSCBundle _ [Right message]) -> return message
    let getTimestamp = receiveOSCBundle $ \(OSCBundle timestamp _) -> return timestamp
    result <- continuation MkTestContext{receiveOSCBundle, getMessage, getTimestamp}
    killThread clientThread
    return result

makeSuperDirtMessage :: BS.ByteString -> OSC
makeSuperDirtMessage sound = OSC "/play2" message
  where
    message :: [OSCDatum]
    message = [OSC_S "s", OSC_S sound]

spec :: Spec
spec = do
  describe "OSC backend" $ do
    let bpm = 240
    let signal = nest [embed [makeSuperDirtMessage "bd"], embed [makeSuperDirtMessage "sn"]]

    it "sends events with the right data" $ do
      withMockOSC signal bpm $ \MkTestContext{getMessage} -> do
        message <- getMessage
        message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])

        message <- getMessage
        message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])

    it "sends events at the right tempo, with an average jitter of less than 500ns" $ do
      withMockOSC signal bpm $ \MkTestContext{getTimestamp} -> do
        deltas <- (sequence $ replicate 12 $ getTimestamp)
          & fmap (\timestamps -> zipWith diffTimestamp (tail timestamps) timestamps)
        let
          expectedTimeDifference :: Double
          expectedTimeDifference = 60/fromIntegral bpm/2
        let
          error :: [Double]
          error = zipWith (\x y -> abs(x - y)) (repeat expectedTimeDifference) deltas
        mean error `shouldBeLessThan` 0.5e-6

    it "sends events with timestamps with less than 2ms of latency" $ do
      withMockOSC signal bpm $ \MkTestContext{getTimestamp} -> do
        timestamp <- getTimestamp
        now <- utcToTimestamp <$> Time.getCurrentTime
        (abs $ now `diffTimestamp` timestamp) `shouldBeLessThan` 2e-3

        timestamp <- getTimestamp
        now <- utcToTimestamp <$> Time.getCurrentTime
        (abs $ now `diffTimestamp` timestamp) `shouldBeLessThan` 2e-3

        timestamp <- getTimestamp
        now <- utcToTimestamp <$> Time.getCurrentTime
        (abs $ now `diffTimestamp` timestamp) `shouldBeLessThan` 2e-3
