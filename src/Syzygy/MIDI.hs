{-# LANGUAGE FunctionalDependencies #-}
module Syzygy.MIDI where

import Control.Monad (forever, void)
import Control.Concurrent
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

import Syzygy hiding (Config(..), Env(..), makeEnv)
import Syzygy.Core

getAddress :: (SndSeq.OpenMode mode) => SndSeq.T mode -> String -> (Addr.T -> IO ()) -> IO ()
getAddress h expectedPortName continuation = do
  ClientInfo.queryLoop_ h $ \cinfo -> do -- for all clients:
    client <- ClientInfo.getClient cinfo
    PortInfo.queryLoop_ h client $ \portInfo -> do -- for all ports:
      portName <- PortInfo.getName portInfo
      if
        portName == expectedPortName
      then
        do
          port <- PortInfo.getPort portInfo
          continuation (Addr.Cons client port)
      else
        return ()

connectTo :: String -> ((SndSeq.T SndSeq.DuplexMode, Addr.T) -> IO ()) -> IO ()
connectTo expectedPortName continuation = do
  SndSeq.withDefault SndSeq.Block $ \(h :: SndSeq.T SndSeq.DuplexMode) -> do
    client <- Client.getId h
    Client.setName h ("Syzygy MIDI Client")
    let portCap = (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite, Port.capSubsWrite])
    Port.withSimple h "Output" portCap Port.typeHardware $ \port -> do
      let address = Addr.Cons client port
      getAddress h expectedPortName $ \sinkAddress -> do
        Connect.withTo h port sinkAddress $ \_ -> do
          continuation (h, address)

listen :: (String, String) ->IO () -> (MIDIEvent.T -> IO ()) -> IO ()
listen (clientName, portName) onReady eventHandler = SndSeq.withDefault SndSeq.Block $ \(h :: SndSeq.T SndSeq.InputMode) -> do
  Client.setName h clientName
  Port.withSimple h portName (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \_ -> do
    onReady
    forever $ do
      event <- MIDIEvent.input h
      eventHandler event

makeNote :: Word8 -> MIDIEvent.Data
makeNote pitch = MIDIEvent.NoteEv MIDIEvent.NoteOn (MIDIEvent.simpleNote (MIDIEvent.Channel 0) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

data MIDIConfig = MkMIDIConfig
  { midiPortName :: String
  , bpmRef :: MVar Int
  , signalRef :: MVar (Signal Word8)
  , clockRef :: MVar Rational
  }

makeMIDIEnv' :: MIDIConfig -> (Env Word8 -> IO ()) -> IO ()
makeMIDIEnv' MkMIDIConfig { midiPortName } continuation = connectTo midiPortName $ \(h, address) ->
  let
    _sendNote :: Word8 -> IO ()
    _sendNote pitch = void $ MIDIEvent.output h $ MIDIEvent.simple address (makeNote pitch)

    _tick :: IO ()
    _tick = void $ MIDIEvent.output h $ MIDIEvent.simple address $ MIDIEvent.QueueEv (MIDIEvent.QueueClock) Queue.direct

    _drain :: IO ()
    _drain = void $ MIDIEvent.drainOutput h

    sendEvents :: Rational -> [Event Word8] -> IO ()
    sendEvents _ events = do
      let midiEvents = (\MkEvent {payload} -> payload) <$> events
      _ <- traverse _sendNote midiEvents
      _tick
      _drain
  in
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
    toCoreConfig MkMIDIConfig{bpmRef, signalRef, clockRef} = MkCoreConfig{bpmRef, signalRef, clockRef}

    makeEnv :: MIDIConfig -> IO (Env Word8)
    makeEnv = makeMIDIEnv

main :: IO ()
main = do
  signalRef <- newMVar (embed 30)
  clockRef <- newMVar 0
  bpmRef <- newMVar 120
  let midiPortName = "UM-ONE MIDI 1"
  let config = MkMIDIConfig { bpmRef, midiPortName, signalRef, clockRef}
  runBackend backend config
