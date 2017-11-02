module Syzygy.MIDISpec where

import Control.Concurrent
import Control.Monad
import Data.Word (Word8)
import Data.Function
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Time as MIDITime
import qualified Sound.ALSA.Sequencer.RealTime as MIDIRealTime
import Test.Hspec

import Syzygy.Core
import Syzygy.MIDI
import Syzygy.Signal
import TestUtils (shouldBeLessThan, mean, doUntil)

data TestContext = MkTestContext
  { onEvent :: forall a. (MIDIEvent.T -> IO a) -> IO a
  , getNoteEvent :: IO MIDIEvent.T
  }

listen :: String -> String -> IO () -> (MIDIEvent.T -> IO ()) -> IO ()
listen clientName portName onReady eventHandler = SndSeq.withDefault SndSeq.Block $ \(h :: SndSeq.T SndSeq.InputMode) -> do
  Client.setName h clientName
  Port.withSimple h portName (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \_ -> do
    onReady
    forever $ do
      event <- MIDIEvent.input h
      eventHandler event

withMockMIDIServer :: CoreConfig_ MIDIEvent.Data -> (TestContext -> IO a) -> IO a
withMockMIDIServer coreConfig continuation = do
  (isReadySem :: MVar ()) <- newEmptyMVar
  (midiEventRef :: MVar MIDIEvent.T) <- newEmptyMVar
  listenerThread <- forkIO $ do
    let
      readyComputation :: IO ()
      readyComputation = putMVar isReadySem ()
    let
      handleEvent :: MIDIEvent.T -> IO ()
      handleEvent = (\event -> putMVar midiEventRef event)
    listen "Syzygy test client" "Syzygy test port" readyComputation handleEvent
  let
    waitForReadyComputation :: IO ()
    waitForReadyComputation = takeMVar isReadySem
  waitForReadyComputation
  clientThread <- forkIO $ do
    let mockMIDIConfig = MkMIDIConfig{midiPortName="Syzygy test port"}
    backend <- makeMIDISendTimestampedEvents mockMIDIConfig
    runBackend (fromSendTimestampedEvents backend) coreConfig
  let
    onEvent :: (MIDIEvent.T -> IO a) -> IO a
    onEvent handleEvent = do
      event <- takeMVar midiEventRef
      handleEvent event
  let
    getNoteEvent :: IO MIDIEvent.T
    getNoteEvent = doUntil isNoteEvent (onEvent return)
  result <- continuation MkTestContext{onEvent, getNoteEvent}
  killThread listenerThread
  killThread clientThread
  return result

getPitch :: MIDIEvent.T -> Word8
getPitch (MIDIEvent.body -> MIDIEvent.NoteEv _ note) = MIDIEvent.unPitch (MIDIEvent.noteNote note)
getPitch (MIDIEvent.body -> _ ) = error "not a note"

getNoteEvTag :: MIDIEvent.T -> MIDIEvent.NoteEv
getNoteEvTag (MIDIEvent.body -> MIDIEvent.NoteEv noteEv _) = noteEv
getNoteEvTag (MIDIEvent.body -> _ ) = error "not a note"

getNoteTime :: MIDIEvent.T -> Integer
getNoteTime MIDIEvent.Cons{time = MIDITime.Cons MIDITime.Absolute (MIDITime.Real time)} = MIDIRealTime.toInteger time
getNoteTime _ = error "not absolute time"

isNoteEvent :: MIDIEvent.T -> Bool
isNoteEvent event = case MIDIEvent.body event of
  MIDIEvent.NoteEv _ _ -> True
  _ -> False

makeDefaultCoreConfig :: IO (CoreConfig_ MIDIEvent.Data)
makeDefaultCoreConfig = do
  signalRef <- newMVar $ switch
    [ embed (makeNoteOnData 0 100)
    , embed (makeNoteOffData 0 100)
    , embed (makeNoteOnData 0 200)
    , embed (makeNoteOffData 0 200)
    ]
  beatRef <- newMVar 0
  bpmRef <- newMVar 240
  return MkCoreConfig { bpmRef, signalRef, beatRef}

spec :: Spec
spec = do
  describe "MIDI SendTimestampedEvents'" $ do
    it "sends MIDI events with the right notes" $ do
      coreConfig <- makeDefaultCoreConfig
      withMockMIDIServer coreConfig $ \MkTestContext{getNoteEvent} -> do
        event <- getNoteEvent
        getPitch event `shouldBe` 100
        getNoteEvTag event `shouldBe` MIDIEvent.NoteOn

        event <- getNoteEvent
        getPitch event `shouldBe` 100
        getNoteEvTag event `shouldBe` MIDIEvent.NoteOff

        event <- getNoteEvent
        getPitch event `shouldBe` 200
        getNoteEvTag event `shouldBe` MIDIEvent.NoteOn

        event <- getNoteEvent
        getPitch event `shouldBe` 200
        getNoteEvTag event `shouldBe` MIDIEvent.NoteOff

    it "sends MIDI events at the right tempo, with jitter of less than 20ns" $ do
      coreConfig <- makeDefaultCoreConfig
      withMockMIDIServer coreConfig $ \MkTestContext{getNoteEvent} -> do
        timeDifferences <- sequence [getNoteEvent | _ <- [1..10]]
          & (fmap . fmap) getNoteTime
          & fmap (\times -> zipWith (-) (tail times) times)
        let
          expectedTimeDifference :: Integer
          expectedTimeDifference = (10^9) `div` (240 `div` 60)
        let
          error :: [Double]
          error = timeDifferences
              & fmap (\delta -> abs (expectedTimeDifference - delta))
              & fmap fromInteger
        mean error `shouldBeLessThan` 20
