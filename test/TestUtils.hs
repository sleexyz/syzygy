module TestUtils where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Test.Hspec
import Vivid.OSC (OSCBundle(..), decodeOSCBundle, Timestamp(..))

import qualified Test.Hspec.Expectations
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS

shouldBeLessThan :: (HasCallStack, Show a, Ord a) => a -> a -> IO ()
shouldBeLessThan x y =
  if x < y
  then return ()
  else Test.Hspec.Expectations.expectationFailure
    $ "expected "
    <> show x
    <> " to be less than "
    <> show y


-- TODO: Kill thread
-- | Serves a mock OSC server over UDP
withMockOSCServer :: (OSCBundle -> IO ()) -> (Network.PortNumber -> IO a) -> IO a
withMockOSCServer handleOSCBundle cont = do
  address <- head <$> Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show Network.aNY_PORT))
  socket <- Network.socket (Network.addrFamily address) Network.Datagram Network.defaultProtocol
  Network.bind socket (Network.addrAddress address)
  _ <- forkIO $ forever $ do
    msg <- NetworkBS.recv socket 4096
    let Right bundle = decodeOSCBundle msg -- NOTE: partial!
    handleOSCBundle bundle
  portNumber <- Network.socketPort socket
  cont portNumber

diffTimestamp :: Timestamp -> Timestamp -> Double
diffTimestamp (Timestamp x) (Timestamp y) = x - y

