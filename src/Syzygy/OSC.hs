module Syzygy.OSC where

import Data.Function ((&))
import Control.Arrow (first)
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import qualified Vivid.OSC as OSC

import Syzygy.Signal
import Syzygy.Core

toOSCBundle :: (Integer, [OSC.OSC]) -> BS.ByteString
toOSCBundle (timeNs, message) = OSC.encodeOSCBundle $ OSC.OSCBundle timestamp (Right <$> message)
  where
    timestamp = OSC.Timestamp (fromInteger timeNs / 10^9)

data OSCConfig = MkOSCConfig
  { portNumber :: Network.PortNumber
  }

-- | nanoseconds between 1970-01-01 and 1900-01-01
epochOffset :: Integer
epochOffset = 2208988800 * 10^9

makeLocalUDPConnection :: Network.PortNumber -> IO Network.Socket
makeLocalUDPConnection portNumber = do
  (a:_) <- Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNumber))
  socket <- Network.socket (Network.addrFamily a) Network.Datagram Network.defaultProtocol
  Network.connect socket (Network.addrAddress a)
  return socket

makeOSCTimestampedEventDispatcher :: OSCConfig -> IO (TimestampedEventDispatcher [OSC.OSC])
makeOSCTimestampedEventDispatcher MkOSCConfig{portNumber} = do
  socket <- makeLocalUDPConnection portNumber
  let
    sendEvents :: [ (Integer, [OSC.OSC]) ] -> IO ()
    sendEvents events = do
      let
        correctedEvents :: [(Integer, [OSC.OSC])]
        correctedEvents = events
          & (fmap . first) (+epochOffset)
      _ <- traverse (NetworkBS.send socket . toOSCBundle) correctedEvents
      return ()
  return sendEvents

main :: IO ()
main = do
  bpmRef <-  newMVar 120
  signalRef <- newMVar $ fast 16 $ embed [OSC.OSC "/play2" [OSC.OSC_S "s", OSC.OSC_S "bd"]]
  beatRef <- newMVar 0
  let portNumber = 57120
  let coreConfig = MkCoreConfig { bpmRef, signalRef, beatRef }
  timestampedEventDispatcher <- makeOSCTimestampedEventDispatcher MkOSCConfig { portNumber }
  runEventDispatcher (liftTimestampedEventDispatcher timestampedEventDispatcher) coreConfig
