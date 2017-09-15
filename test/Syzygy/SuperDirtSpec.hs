
module Syzygy.SuperDirtSpec where

import Control.Concurrent
import TestUtils
  (
    shouldBeLessThan
  , withMockOSCServer
  , diffTimestamp
  )
import Test.Hspec
import Vivid.OSC (OSCBundle(..), OSCDatum(OSC_S), OSC(..), Timestamp )

import Syzygy.SuperDirt
import Syzygy.Signal
import Syzygy.Core
import qualified Data.ByteString as BS

data TestContext = MkTestContext { receiveOSCBundle :: (OSCBundle -> IO ()) -> IO () }

_withOSCHandler :: Signal BS.ByteString -> (OSCBundle -> IO ()) -> IO SuperDirtConfig
_withOSCHandler defaultSignal handler = withMockOSCServer handler $ \superDirtPortNumber -> do
  bpmRef <- newMVar 60
  signalRef <- newMVar defaultSignal
  clockRef <- newMVar 0
  return MkSuperDirtConfig{superDirtPortNumber, bpmRef, signalRef, clockRef}

withMockSuperDirt :: Signal BS.ByteString -> (TestContext -> IO ()) -> IO ()
withMockSuperDirt defaultSignal cont = do
  (bundleChan :: MVar OSCBundle) <- newEmptyMVar
  config <- _withOSCHandler defaultSignal $ putMVar bundleChan
  clientThread <- forkIO $ runBackend backend config
  let receiveOSCBundle cont = do
        bundleVal <- takeMVar bundleChan
        cont bundleVal
  cont MkTestContext{receiveOSCBundle}
  killThread clientThread


spec :: Spec
spec = do
  describe "SuperDirt backend" $ do
    it "can send events" $ do
      let signal = nest [embed "bd", embed "sn"]
      withMockSuperDirt signal $ \MkTestContext{receiveOSCBundle} -> do
        receiveOSCBundle $ \(OSCBundle _ [Right message]) -> do
          message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
        receiveOSCBundle $ \(OSCBundle _ [Right message]) -> do
          message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])

    it "sends events with the right timestamps" $ do
      let signal = nest [embed "bd", embed "sn"]
      (timestampsRef :: MVar [Vivid.OSC.Timestamp]) <- newMVar []
      withMockSuperDirt signal $ \MkTestContext{receiveOSCBundle} -> do
        sequence_ $ replicate 4 $ receiveOSCBundle $ \(OSCBundle timestamp _) -> do
          modifyMVar_ timestampsRef (\xs -> return $ timestamp:xs)

      timestamps <- readMVar timestampsRef
      let delays = zipWith diffTimestamp timestamps (drop 1 timestamps)
      let expectedDelays = repeat 0.5
      let deltas = zipWith (-) delays expectedDelays
      sum deltas `shouldBeLessThan` 0.1
