module Syzygy.OSC where

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
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal [OSC.OSC])
  , beatRef :: MVar Rational
  }

epochOffset :: Integer
epochOffset = 2208988800 * 10^9

toAbsoluteTime :: Int -> Rational -> Integer -> Event a -> (Integer, a)
toAbsoluteTime bpm beatStart clock MkEvent{interval=(eventStart, _), payload} =
  let
    offset :: Integer
    offset = floor $ (10^9 * 60) * (eventStart - beatStart) / fromIntegral bpm

    time :: Integer
    time = clock + offset + epochOffset
  in
    (time, payload)

makeLocalUDPConnection :: Network.PortNumber -> IO Network.Socket
makeLocalUDPConnection portNumber = do
  (a:_) <- Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNumber))
  socket <- Network.socket (Network.addrFamily a) Network.Datagram Network.defaultProtocol
  Network.connect socket (Network.addrAddress a)
  return socket

makeOSCEnv :: OSCConfig -> IO (Env [OSC.OSC])
makeOSCEnv MkOSCConfig{portNumber, bpmRef} = do
  socket <- makeLocalUDPConnection portNumber
  let
    sendEvents :: Rational -> Integer -> [ Event [OSC.OSC] ] -> IO ()
    sendEvents beat clock events = do
      bpm <- readMVar bpmRef
      let oscEvents = [toAbsoluteTime bpm beat clock event | event <- events]
      _ <- traverse (NetworkBS.send socket . toOSCBundle) oscEvents
      return ()
  return MkEnv { sendEvents }

backend :: Backend OSCConfig [OSC.OSC]
backend = MkBackend {toCoreConfig, makeEnv}
  where
    toCoreConfig :: OSCConfig -> CoreConfig [OSC.OSC]
    toCoreConfig MkOSCConfig {bpmRef, signalRef, beatRef} =
      MkCoreConfig {bpmRef, signalRef, beatRef}

    makeEnv :: OSCConfig -> IO (Env [OSC.OSC])
    makeEnv = makeOSCEnv

main :: IO ()
main = do
  bpmRef <-  newMVar 60
  signalRef <- newMVar mempty
  beatRef <- newMVar 0
  let portNumber = 57120
  let config = MkOSCConfig { bpmRef, signalRef, beatRef, portNumber }
  runBackend backend config
