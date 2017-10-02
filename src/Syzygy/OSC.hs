module Syzygy.OSC where

import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.Time as Time
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import qualified Vivid.OSC as OSC

import Syzygy.Signal
import Syzygy.Core

toOSCBundle :: (Time.UTCTime, [OSC.OSC]) -> BS.ByteString
toOSCBundle (time, message) = OSC.encodeOSCBundle $ OSC.OSCBundle (OSC.utcToTimestamp time) (Right <$> message)

data OSCConfig = MkOSCConfig
  { portNumber :: Network.PortNumber
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal [OSC.OSC])
  , beatRef :: MVar Rational
  }

toAbsoluteTime :: Time.UTCTime -> Rational -> Int -> Event a -> (Time.UTCTime, a)
toAbsoluteTime now clockVal bpm MkEvent{interval=(eventStart, _), payload} =
  let
    delay = (eventStart - clockVal) * 60 / fromIntegral bpm
    timestamp = Time.addUTCTime (fromRational delay) now
  in
    (timestamp, payload)

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
    sendEvents clockVal _ events = do
      now <- Time.getCurrentTime
      bpm <- readMVar bpmRef
      let oscEvents = [toAbsoluteTime now clockVal bpm event | event <- events]
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
