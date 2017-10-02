module Syzygy.OSCSpec where

import Control.Concurrent
import TestUtils (shouldBeLessThan, mean)
import Data.Function ((&))
import Test.Hspec
import Vivid.OSC (OSCBundle(..), decodeOSCBundle, OSCDatum(OSC_S), OSC(..), Timestamp(..) )
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
    let config = MkOSCConfig{portNumber, bpmRef, signalRef, beatRef}
    clientThread <- forkIO $ runBackend backend config
    let receiveOSCBundle bundleHandler = do
          bundleVal <- takeMVar bundleChan
          bundleHandler bundleVal
    let getMessage = receiveOSCBundle $ \(OSCBundle _ [Right message]) -> return message
    result <- continuation MkTestContext{receiveOSCBundle, getMessage}
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

    it "sends events at the right tempo, with an average jitter of less than 1ms" $ do
      withMockOSC signal bpm $ \MkTestContext{receiveOSCBundle} -> do
        deltas <- (sequence $ replicate 12 $ receiveOSCBundle $ \(OSCBundle timestamp _) -> return timestamp)
          & fmap (\timestamps -> zipWith diffTimestamp (tail timestamps) timestamps)
        let
          expectedTimeDifference :: Double
          expectedTimeDifference = 60/fromIntegral bpm/2
        let
          error :: [Double]
          error = zipWith (\x y -> abs(x - y)) (repeat expectedTimeDifference) deltas
        mean error `shouldBeLessThan` 1e-3
