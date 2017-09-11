{-# LANGUAGE FunctionalDependencies #-}
module Syzygy.MIDI where

import Control.Monad (forever, void)
import Control.Concurrent
import Data.Word (Word8)
import qualified Sound.ALSA.Exception as AlsaExc
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

handleNativeExceptions :: IO () -> IO ()
handleNativeExceptions action =
   action `AlsaExc.catch` \e ->
     putStrLn $ "alsa_exception: " ++ AlsaExc.show e

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
    Client.setName h ("Alsa Test")
    let portCap = (Port.caps [Port.capRead, Port.capSubsRead, Port.capWrite, Port.capSubsWrite])
    Port.withSimple h "Output" portCap Port.typeHardware $ \port -> do
      let address = Addr.Cons client port
      getAddress h expectedPortName $ \sinkAddress -> do
        Connect.withTo h port sinkAddress $ \_ -> do
          continuation (h, address)

makeNote :: Word8 -> MIDIEvent.Data
makeNote pitch = MIDIEvent.NoteEv MIDIEvent.NoteOn (MIDIEvent.simpleNote (MIDIEvent.Channel 0) (MIDIEvent.Pitch pitch) (MIDIEvent.Velocity 255))

data MIDIConfig = MkMIDIConfig
  { midiPortName :: String
  , tpb :: Int
  , bpm :: Int
  , signalRef :: MVar (Signal Word8)
  , clockRef :: MVar Rational
  }

data MIDIEnv = MkMIDIEnv
  { sendEvents :: Rational -> [Event Word8] -> IO ()
  }

_makeEnv' :: MIDIConfig -> (MIDIEnv -> IO ()) -> IO ()
_makeEnv' MkMIDIConfig { midiPortName } continuation = connectTo midiPortName $ \(h, address) ->
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
    do
      continuation MkMIDIEnv {sendEvents}

makeEnv :: MIDIConfig -> IO MIDIEnv
makeEnv config = do
  (envRef :: MVar (Maybe MIDIEnv)) <- newEmptyMVar
  void $ forkIO $ do
    _makeEnv' config $ \env -> do
      putMVar envRef $ Just env
      forever (threadDelay 1000000000)
    putMVar envRef Nothing
  maybeEnv <- takeMVar envRef
  case maybeEnv of
    Just env -> return env
    Nothing -> error "device not found"

data CoreConfig a = MkCoreConfig
  { bpm :: Int
  , tpb :: Int
  , signalRef :: MVar (Signal a)
  , clockRef :: MVar Rational
  }


data Backend config a = MkBackend
  { toCoreConfig :: config -> CoreConfig a
  , makeSendEvents :: config -> IO (Rational -> [Event a] -> IO ())
  }

runBackend :: Backend config a -> config -> IO ()
runBackend MkBackend {toCoreConfig, makeSendEvents} config = do
  let MkCoreConfig { bpm, tpb, signalRef, clockRef } = toCoreConfig config
  sendEvents <- makeSendEvents config
  forever $ do
    sig <- readMVar signalRef
    clockVal <- modifyMVar clockRef (\x -> return (x + (1/fromIntegral tpb), x))
    let events = signal (pruneSignal sig) (clockVal, clockVal + (1/fromIntegral tpb))
    sendEvents clockVal events
    delayOneBeat bpm tpb


midiBackend :: Backend MIDIConfig Word8
midiBackend = MkBackend {toCoreConfig, makeSendEvents}
  where
    toCoreConfig MkMIDIConfig{tpb, bpm, signalRef, clockRef} = MkCoreConfig{tpb, bpm, signalRef, clockRef}
    makeSendEvents config = do
      MkMIDIEnv{sendEvents} <- makeEnv config
      return sendEvents

main :: IO ()
main = do
  signalRef <- newMVar mempty
  clockRef <- newMVar 0
  let bpm = 60
  let tpb = 24
  let midiPortName = "UM-ONE MIDI 1"
  let config = MkMIDIConfig { tpb, bpm, midiPortName, signalRef, clockRef}
  modifyMVar_ signalRef $ (const $ return $ embed 30)
  runBackend midiBackend config
