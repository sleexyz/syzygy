module Syzygy.OSCSpec where

import Control.Concurrent
import TestUtils (shouldBeLessThan)
import Test.Hspec
import Vivid.OSC (OSCBundle(..), decodeOSCBundle, OSCDatum(OSC_S), OSC(..), Timestamp(..) )
import Control.Monad

import Syzygy.OSC
import Syzygy.Signal
import Syzygy.Core
import qualified Data.ByteString as BS
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS

diffTimestamp :: Timestamp -> Timestamp -> Double
diffTimestamp (Timestamp x) (Timestamp y) = x - y

data TestContext = MkTestContext { receiveOSCBundle :: (OSCBundle -> IO ()) -> IO () }

withMockOSCServer :: (OSCBundle -> IO ()) -> (Network.PortNumber -> IO ())  -> IO ()
withMockOSCServer handleOSCBundle continuation = do
  address <- head <$> Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show Network.aNY_PORT))
  socket <- Network.socket (Network.addrFamily address) Network.Datagram Network.defaultProtocol
  Network.bind socket (Network.addrAddress address)
  threadId <- forkIO $ forever $ do
    msg <- NetworkBS.recv socket 4096
    let Right bundle = decodeOSCBundle msg
    handleOSCBundle bundle
  portNumber <- Network.socketPort socket
  continuation portNumber
  killThread threadId
  return ()

withMockOSC :: Signal BS.ByteString -> Int -> (TestContext -> IO ()) -> IO ()
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
    continuation MkTestContext{receiveOSCBundle}
    killThread clientThread

spec :: Spec
spec = do
  describe "OSC backend" $ do
    it "can send events" $ do
      let bpm = 60
      let signal = nest [embed "bd", embed "sn"]
      withMockOSC signal bpm $ \MkTestContext{receiveOSCBundle} -> do
        receiveOSCBundle $ \(OSCBundle _ [Right message]) -> message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
        receiveOSCBundle $ \(OSCBundle _ [Right message]) -> message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])

    it "sends events with the right timestamps" $ do
      let bpm = 60
      let signal = nest [embed "bd", embed "sn"]
      (timestampsRef :: MVar [Vivid.OSC.Timestamp]) <- newMVar []
      withMockOSC signal bpm $ \MkTestContext{receiveOSCBundle} -> do
        sequence_ $ replicate 4 $ receiveOSCBundle $ \(OSCBundle timestamp _) -> do
          modifyMVar_ timestampsRef (\xs -> return $ timestamp:xs)

      timestamps <- readMVar timestampsRef
      let delays = zipWith diffTimestamp timestamps (drop 1 timestamps)
      let expectedDelays = repeat 0.5
      let deltas = zipWith (-) delays expectedDelays
      sum deltas `shouldBeLessThan` 0.1
