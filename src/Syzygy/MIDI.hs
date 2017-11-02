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
import qualified System.Clock as Clock

import Syzygy.Core

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
  }

stamp :: Addr.T -> Queue.T -> Integer -> MIDIEvent.Data -> MIDIEvent.T
stamp address queue nanosecs body = (MIDIEvent.simple address body)
  { MIDIEvent.queue = queue
  , MIDIEvent.time = ALSATime.consAbs $ ALSATime.Real $ ALSARealTime.fromInteger nanosecs
  }

makeNoteOnData :: Word8 -> Word8 -> MIDIEvent.Data
makeNoteOnData channel pitch = MIDIEvent.NoteEv MIDIEvent.NoteOn (MIDIEvent.simpleNote (MIDIEvent.Channel channel) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

makeNoteOffData :: Word8 -> Word8 -> MIDIEvent.Data
makeNoteOffData channel pitch = MIDIEvent.NoteEv MIDIEvent.NoteOff (MIDIEvent.simpleNote (MIDIEvent.Channel channel) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 0))

makeCtrlMessage :: Word8 -> Word8 -> MIDIEvent.Data
makeCtrlMessage param value = MIDIEvent.CtrlEv MIDIEvent.Controller $ MIDIEvent.Ctrl (MIDIEvent.Channel 0) (MIDIEvent.Parameter $ fromIntegral param) (MIDIEvent.Value $ fromIntegral value)

_makeMIDISendTimestampedEvents :: MIDIConfig -> (SendTimestampedEvents MIDIEvent.Data -> IO ()) -> IO ()
_makeMIDISendTimestampedEvents MkMIDIConfig{midiPortName} continuation = connectTo midiPortName $ \h address queue -> let
  sendEvents :: [(Integer, MIDIEvent.Data)] -> IO ()
  sendEvents events = do
    let
      stampEvent :: (Integer, MIDIEvent.Data) -> MIDIEvent.T
      stampEvent (time, payload) = stamp address queue time payload
    let
      notes :: [MIDIEvent.T]
      notes =  stampEvent <$> events
    traverse (MIDIEvent.output h) notes
    MIDIEvent.drainOutput h
    return ()
  in do
    Queue.control h queue MIDIEvent.QueueStart Nothing
    now <- Clock.toNanoSecs <$> Clock.getTime Clock.Realtime
    Queue.control h queue (MIDIEvent.QueueSetPosTime $ ALSARealTime.fromInteger now) Nothing
    continuation sendEvents

makeMIDISendTimestampedEvents :: MIDIConfig -> IO (SendTimestampedEvents MIDIEvent.Data)
makeMIDISendTimestampedEvents config = do
  (envRef :: MVar (Maybe (SendTimestampedEvents MIDIEvent.Data))) <- newEmptyMVar
  void $ forkIO $ do
    _makeMIDISendTimestampedEvents config $ \env -> do
      putMVar envRef $ Just env
      forever (threadDelay 1000000000)
    putMVar envRef Nothing
  maybeEnv <- takeMVar envRef
  case maybeEnv of
    Just env -> return env
    Nothing -> error "Device not found"
