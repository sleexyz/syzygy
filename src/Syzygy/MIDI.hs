module Syzygy.MIDI where

import Control.Concurrent
import Control.Monad (forever, void)
import Data.Word (Word8)
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.RealTime as ALSARealTime
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Time as ALSATime
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Queue as Queue
-- import qualified System.Clock as Clock


import Syzygy.Core
import Syzygy.Signal

getAddress :: (SndSeq.OpenMode mode) => SndSeq.T mode -> String -> (Addr.T -> IO ()) -> IO ()
getAddress h expectedPortName continuation = do
  ClientInfo.queryLoop_ h $ \cinfo -> do
    client <- ClientInfo.getClient cinfo
    PortInfo.queryLoop_ h client $ \portInfo -> do
      portName <- PortInfo.getName portInfo
      if
        portName == expectedPortName
      then
        do
          port <- PortInfo.getPort portInfo
          continuation (Addr.Cons client port)
      else
        return ()

connectTo :: String -> (SndSeq.T SndSeq.DuplexMode -> Addr.T -> Queue.T -> IO ()) -> IO ()
connectTo expectedPortName continuation = do
  SndSeq.withDefault SndSeq.Block $ \(h :: SndSeq.T SndSeq.DuplexMode) -> do
    client <- Client.getId h
    Client.setName h "Syzygy MIDI Client"
    let portCap = (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite, Port.capSubsWrite])
    Port.withSimple h "Output" portCap Port.typeHardware $ \port -> do
      let address = Addr.Cons client port
      getAddress h expectedPortName $ \sinkAddress -> do
        Connect.withTo h port sinkAddress $ \_ -> do
          Queue.with h $ \queue -> do
            continuation h address queue

makeNote :: Word8 -> MIDIEvent.Data
makeNote pitch = MIDIEvent.NoteEv MIDIEvent.NoteOn (MIDIEvent.simpleNote (MIDIEvent.Channel 0) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

data MIDIConfig = MkMIDIConfig
  { midiPortName :: String
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal Word8)
  , clockRef :: MVar Rational
  }

stamp :: Integer -> Queue.T -> MIDIEvent.T -> MIDIEvent.T
stamp nanosecs queue event = event
  {
    MIDIEvent.queue = queue
  , MIDIEvent.time = ALSATime.consRel $ ALSATime.Real $ ALSARealTime.fromInteger nanosecs
  }

makeMIDIEnv' :: MIDIConfig -> (Env Word8 -> IO ()) -> IO ()
makeMIDIEnv' MkMIDIConfig { midiPortName, bpmRef } continuation = connectTo midiPortName $ \h address queue -> do
  let
    sendNote :: (Integer, Word8) -> IO ()
    sendNote (delay, pitch) = do
      let event = stamp delay queue $ MIDIEvent.simple address (makeNote pitch)
      _ <- MIDIEvent.output h event
      return ()

    _tick :: IO ()
    _tick = void $ MIDIEvent.output h $ MIDIEvent.simple address $ MIDIEvent.QueueEv (MIDIEvent.QueueClock) Queue.direct
  let
    sendEvents :: Rational -> [Event Word8] -> IO ()
    sendEvents clockVal events = do
      bpm <- readMVar bpmRef
      -- now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
      let
        extractNote :: Event Word8 -> (Integer, Word8)
        extractNote MkEvent {interval=(eventStart, _), payload} = (nanosecs, payload)
          where
            foo = floor $ (10^9 * 60) * (eventStart - clockVal) / fromIntegral bpm

            nanosecs :: Integer
            nanosecs = foo

        notes :: [(Integer, Word8)]
        notes = extractNote <$> events

      _ <- traverse sendNote notes
      _ <- MIDIEvent.drainOutput h
      return ()
  _ <- Queue.control h queue MIDIEvent.QueueStart Nothing
  -- now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
  -- _ <- Queue.control h queue (MIDIEvent.QueueSetPosTime $ ALSARealTime.fromInteger now) Nothing
  continuation MkEnv {sendEvents}

makeMIDIEnv :: MIDIConfig -> IO (Env Word8)
makeMIDIEnv config = do
  (envRef :: MVar (Maybe (Env Word8))) <- newEmptyMVar
  void $ forkIO $ do
    makeMIDIEnv' config $ \env -> do
      putMVar envRef $ Just env
      forever (threadDelay 1000000000)
    putMVar envRef Nothing
  maybeEnv <- takeMVar envRef
  case maybeEnv of
    Just env -> return env
    Nothing -> error "Device not found"

backend :: Backend MIDIConfig Word8
backend = MkBackend {toCoreConfig, makeEnv}
  where
    toCoreConfig :: MIDIConfig -> CoreConfig Word8
    toCoreConfig MkMIDIConfig{bpmRef, signalRef, clockRef} =
      MkCoreConfig{bpmRef, signalRef, clockRef}

    makeEnv :: MIDIConfig -> IO (Env Word8)
    makeEnv = makeMIDIEnv
