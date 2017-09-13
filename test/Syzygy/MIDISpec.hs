
module Syzygy.MIDISpec where

import Control.Concurrent
import Control.Monad
import Data.Word (Word8)
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent
import qualified Sound.ALSA.Sequencer.Port as Port
import Test.Hspec

import Syzygy
import Syzygy.Core
import Syzygy.MIDI

data TestContext = MkTestContext {onEvent :: forall a. (MIDIEvent.T -> IO a) -> IO a}

makeDefaultConfig :: IO MIDIConfig
makeDefaultConfig = do
  signalRef <- newMVar mempty
  clockRef <- newMVar 0
  bpmRef <- newMVar 60
  let midiPortName = "Syzygy test port"
  let config = MkMIDIConfig { bpmRef, midiPortName, signalRef, clockRef}
  return config

listen :: String -> String -> IO () -> (MIDIEvent.T -> IO ()) -> IO ()
listen clientName portName onReady eventHandler = SndSeq.withDefault SndSeq.Block $ \(h :: SndSeq.T SndSeq.InputMode) -> do
  Client.setName h clientName
  Port.withSimple h portName (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \_ -> do
    onReady
    forever $ do
      event <- MIDIEvent.input h
      eventHandler event


withMockMIDIServer :: MIDIConfig -> (TestContext -> IO ()) -> IO ()
withMockMIDIServer config continuation = do
  (isReadySem :: MVar ()) <- newEmptyMVar
  (midiEventRef :: MVar MIDIEvent.T) <- newEmptyMVar

  listenerThread <- forkIO $ do
    let
      readyComputation :: IO ()
      readyComputation = putMVar isReadySem ()

      handleEvent :: MIDIEvent.T -> IO ()
      handleEvent = (\event -> putMVar midiEventRef event)

    listen "Syzygy test client" "Syzygy test port" readyComputation handleEvent

  let
    waitForReadyComputation :: IO ()
    waitForReadyComputation = takeMVar isReadySem

  waitForReadyComputation
  clientThread <- forkIO $ do
    runBackend backend config

  let
    onEvent :: (MIDIEvent.T -> IO a) -> IO a
    onEvent handleEvent = do
      event <- takeMVar midiEventRef
      handleEvent event

  continuation MkTestContext{onEvent}

  killThread listenerThread
  killThread clientThread

getPitch :: MIDIEvent.Note -> Word8
getPitch note = MIDIEvent.unPitch (MIDIEvent.noteNote note)

spec :: Spec
spec = do
  describe "MIDI out" $ do
    it "can send MIDI signals" $ do
      config@MkMIDIConfig{signalRef} <- makeDefaultConfig
      modifyMVar_ signalRef (const $ return $ embed 60)
      withMockMIDIServer config $ \MkTestContext{onEvent} -> do
        events <- sequence $ replicate 24 $ onEvent return
        let
          filteredData :: [MIDIEvent.Data]
          filteredData = do
            event <- events
            case MIDIEvent.body event of
              body@(MIDIEvent.NoteEv _ _) -> return body
              _ -> fail "not event"

          note :: MIDIEvent.Note
          [MIDIEvent.NoteEv _ note] = filteredData

        getPitch note `shouldBe` 60
