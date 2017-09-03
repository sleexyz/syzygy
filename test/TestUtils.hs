module TestUtils where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Test.Hspec
import Vivid.OSC (OSCBundle(..), OSCDatum(OSC_S), decodeOSCBundle, OSC(..), utcToTimestamp, Timestamp(..))

import qualified Test.Hspec.Expectations
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS

shouldBeAround :: (HasCallStack, Ord a, Num a, Show a) => a -> (a, a) -> IO ()
shouldBeAround value (expectedValue, tolerance) =
  let difference = abs (value - expectedValue)
  in
    if difference < tolerance
    then return ()
    else Test.Hspec.Expectations.expectationFailure
      $ "expected "
      <> show value
      <> " to equal "
      <> show expectedValue
      <> " with a tolerance of "
      <> show tolerance
      <> ", when a difference of "
      <> show difference <> " was found"

-- | Serves a mock OSC server over UDP
withMockOSCServer :: (OSCBundle -> IO ()) -> (Network.PortNumber -> IO a) -> IO a
withMockOSCServer handleOSCBundle cont = do
  address <- head <$> Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show Network.aNY_PORT))
  socket <- Network.socket (Network.addrFamily address) Network.Datagram Network.defaultProtocol
  Network.bind socket (Network.addrAddress address)
  forkIO $ forever $ do
    msg <- NetworkBS.recv socket 4096
    let Right bundle = decodeOSCBundle msg -- NOTE: partial!
    handleOSCBundle bundle
  portNumber <- Network.socketPort socket
  cont portNumber

diffTimestamp :: Timestamp -> Timestamp -> Double
diffTimestamp (Timestamp x) (Timestamp y) = x - y

