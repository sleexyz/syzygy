module Syzygy.MIDI where

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, MVar, newMVar, readMVar, modifyMVar, modifyMVar_)
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

data Config = MkConfig
  { portName :: String

  }

data Env = MkEnv
  { send :: Word8 -> IO ()
  , tick :: IO ()
  , drain :: IO ()
  , signalRef :: MVar (Signal Word8)
  , clockRef :: MVar Rational
  }

makeEnv :: Config -> (Env -> IO ()) -> IO ()
makeEnv MkConfig { portName } continuation = connectTo portName $ \(h, address) ->
  let
    send pitch = void $ MIDIEvent.output h $ MIDIEvent.simple address (makeNote pitch)
    tick = void $ MIDIEvent.output h $ MIDIEvent.simple address $ MIDIEvent.QueueEv (MIDIEvent.QueueClock) Queue.direct
    drain = void $ MIDIEvent.drainOutput h
  in
    do
      signalRef <- newMVar mempty
      clockRef <- newMVar 0
      continuation MkEnv {send, tick, drain, signalRef, clockRef}


delayOnePulse :: Int -> IO ()
delayOnePulse bpm = threadDelay ((60 * 10^6) `div` bpm `div` 24) -- 24 pulses per quarter note

main :: IO ()
main = makeEnv MkConfig { portName = "UM-ONE MIDI 1" } $ \env ->
  let
    bpm = 160
    MkEnv{ send, tick, drain, signalRef, clockRef} = env
  in
    do
      let pat = fast 4 (embed 30)
      modifyMVar_ signalRef (const $ return $ pruneSignal pat)
      forever $ do
        sig <- readMVar signalRef
        clockVal <- modifyMVar clockRef (\x -> return (x + (1/((24))), x))
        _ <- traverse send $ (\MkEvent {payload} -> payload) <$> signal sig (clockVal, clockVal + (1/(24)))
        tick
        drain
        delayOnePulse bpm
