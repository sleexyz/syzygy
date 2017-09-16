module Syzygy.MIDI where

import Control.Concurrent
import Control.Monad (forever, void)
import Data.Word (Word8)
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Address as Addr
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

makeNote :: Word8 -> MIDIEvent.Data
makeNote pitch = MIDIEvent.NoteEv MIDIEvent.NoteOn (MIDIEvent.simpleNote (MIDIEvent.Channel 0) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

data MIDIConfig = MkMIDIConfig
  { midiPortName :: String
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal Word8)
  , clockRef :: MVar Rational
  }

makeMIDIEnv' :: MIDIConfig -> (Env Word8 -> IO ()) -> IO ()
makeMIDIEnv' MkMIDIConfig { midiPortName, bpmRef } continuation = connectTo midiPortName $ \h address _ -> do
  let
    _sendNote :: Word8 -> IO ()
    _sendNote pitch = do
      let event = MIDIEvent.simple address (makeNote pitch)
      void $ MIDIEvent.output h event

    _tick :: IO ()
    _tick = void $ MIDIEvent.output h $ MIDIEvent.simple address $ MIDIEvent.QueueEv (MIDIEvent.QueueClock) Queue.direct

    _drain :: IO ()
    _drain = void $ MIDIEvent.drainOutput h

  (eventChan :: Chan (Int, Word8)) <- newChan
  eventDispatcherThread <- forkIO $ forever $ do
    (delay, payload) <- readChan eventChan
    forkIO $ do
      threadDelay delay
      _sendNote payload
      _drain

  let
    sendEvents :: Rational -> [Event Word8] -> IO ()
    sendEvents clockVal events = do
      bpm <- readMVar bpmRef
      let
        extractNote :: Event Word8 -> (Int, Word8)
        extractNote MkEvent {interval=(start, _), payload} = (floor delay, payload)
          where
            delay :: Rational
            delay = fromIntegral (10^6 * 60) * (start - clockVal) / fromIntegral bpm

        notes :: [(Int, Word8)]
        notes = extractNote <$> events

        _sendNoteToEventChan :: (Int, Word8) -> IO ()
        _sendNoteToEventChan (delay, note) = writeChan eventChan (delay, note)

      _ <- traverse _sendNoteToEventChan notes
      return ()

  continuation MkEnv {sendEvents}
  killThread eventDispatcherThread

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
