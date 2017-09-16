module Syzygy.SuperDirt where

import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.Time as Time
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import qualified Vivid.OSC as OSC

import Syzygy.Signal
import Syzygy.Core

toOSCBundle :: (Time.UTCTime, BS.ByteString) -> BS.ByteString
toOSCBundle (time, sound) = OSC.encodeOSCBundle $ OSC.OSCBundle timestamp [Right $ OSC.OSC "/play2" message] where
    timestamp :: OSC.Timestamp
    timestamp = OSC.utcToTimestamp time

    message :: [OSC.OSCDatum]
    message = [OSC.OSC_S "s", OSC.OSC_S sound]

data SuperDirtConfig = MkSuperDirtConfig
  { superDirtPortNumber :: Network.PortNumber
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal BS.ByteString)
  , clockRef :: MVar Rational
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
  superDirtSocket <- Network.socket (Network.addrFamily a) Network.Datagram Network.defaultProtocol
  Network.connect superDirtSocket (Network.addrAddress a)
  return superDirtSocket

makeSuperDirtEnv :: SuperDirtConfig -> IO (Env BS.ByteString)
makeSuperDirtEnv MkSuperDirtConfig{superDirtPortNumber, bpmRef} = do
  superDirtSocket <- makeLocalUDPConnection superDirtPortNumber
  let
    sendEvents :: Rational -> [ Event BS.ByteString ] -> IO ()
    sendEvents clockVal events = do
      now <- Time.getCurrentTime
      bpm <- readMVar bpmRef
      let oscEvents = [toAbsoluteTime now clockVal bpm event | event <- events]
      _ <- traverse (NetworkBS.send superDirtSocket . toOSCBundle) oscEvents
      return ()
  return MkEnv { sendEvents }

backend :: Backend SuperDirtConfig BS.ByteString
backend = MkBackend {toCoreConfig, makeEnv}
  where
    toCoreConfig :: SuperDirtConfig -> CoreConfig BS.ByteString
    toCoreConfig MkSuperDirtConfig {bpmRef, signalRef, clockRef} =
      MkCoreConfig {bpmRef, signalRef, clockRef}

    makeEnv :: SuperDirtConfig -> IO (Env BS.ByteString)
    makeEnv = makeSuperDirtEnv

main :: IO ()
main = do
  bpmRef <- newMVar 60
  signalRef <- newMVar $ fast 4 $ nest [embed "bd", fast 2 $ embed "bd"]
  clockRef <- newMVar 0
  let superDirtPortNumber = 57120
  let config = MkSuperDirtConfig { bpmRef, signalRef, clockRef, superDirtPortNumber }
  runBackend backend config
