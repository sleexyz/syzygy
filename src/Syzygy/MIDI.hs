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

data MIDIConfig = MkMIDIConfig
  { midiPortName :: String
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal Word8)
  , clockRef :: MVar Rational
  }

stamp :: Queue.T -> Integer -> MIDIEvent.T -> MIDIEvent.T
stamp queue nanosecs event = event
  {
    MIDIEvent.queue = queue
  , MIDIEvent.time = ALSATime.consRel $ ALSATime.Real $ ALSARealTime.fromInteger nanosecs
  }

makeNoteOnData :: Word8 -> MIDIEvent.Data
makeNoteOnData pitch = MIDIEvent.NoteEv MIDIEvent.NoteOn (MIDIEvent.simpleNote (MIDIEvent.Channel 0) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

makeNoteOffData :: Word8 -> MIDIEvent.Data
makeNoteOffData pitch = MIDIEvent.NoteEv MIDIEvent.NoteOff (MIDIEvent.simpleNote (MIDIEvent.Channel 0) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

noteOn :: Addr.T ->  Queue.T -> Integer -> Word8 -> MIDIEvent.T
noteOn address queue delay pitch = stamp queue delay $ MIDIEvent.simple address (makeNoteOnData pitch)

noteOff :: Addr.T ->  Queue.T -> Integer -> Word8 -> MIDIEvent.T
noteOff address queue delay pitch = stamp queue delay $ MIDIEvent.simple address (makeNoteOffData pitch)

makeMIDIEnv' :: MIDIConfig -> (Env Word8 -> IO ()) -> IO ()
makeMIDIEnv' MkMIDIConfig { midiPortName, bpmRef } continuation = connectTo midiPortName $ \h address queue -> do
  let
    sendEvents :: Rational -> [Event Word8] -> IO ()
    sendEvents clockVal events = do
      bpm <- readMVar bpmRef
      let
        extractMIDIEvents :: Event Word8 -> [MIDIEvent.T]
        extractMIDIEvents MkEvent {interval=(eventStart, eventEnd), payload} =
            [ noteOn address queue (getDelay eventStart) payload
            , noteOff address queue (getDelay eventEnd) payload
            ]
          where
            getDelay :: Rational -> Integer
            getDelay val = floor $ (10^9 * 60) * (val - clockVal) / fromIntegral bpm

        notes :: [MIDIEvent.T]
        notes =  events >>= extractMIDIEvents

      _ <- traverse (MIDIEvent.output h)notes
      _ <- MIDIEvent.drainOutput h
      return ()

  _ <- Queue.control h queue MIDIEvent.QueueStart Nothing
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
