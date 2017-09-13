
module Syzygy.MIDISpec where

import Control.Concurrent
import Test.Hspec
import Syzygy.MIDI
import Syzygy.Core
import Syzygy
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent

data TestContext = MkTestContext {onEvent :: forall a. (MIDIEvent.T -> IO a) -> IO a}

makeDefaultConfig :: IO MIDIConfig
makeDefaultConfig = do
  signalRef <- newMVar mempty
  clockRef <- newMVar 0
  bpmRef <- newMVar 60
  let midiPortName = "Syzygy test port"
  let config = MkMIDIConfig { bpmRef, midiPortName, signalRef, clockRef}
  return config

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
    listen ("Syzygy test client", "Syzygy test port") readyComputation handleEvent

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

        MIDIEvent.unPitch (MIDIEvent.noteNote note) `shouldBe` 60
