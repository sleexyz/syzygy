{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
module Live where

import Data.Bifunctor
import Control.Concurrent
import Data.Word (Word8)
import Data.Function ((&))
import Prelude
import Data.String
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent
import qualified Vivid.OSC as OSC

import Syzygy.Core
import Syzygy.Signal
import Syzygy.MIDI
import Syzygy.OSC

setup :: IO (CoreConfig (Either MIDIEvent.Data [OSC.OSC]))
setup = do
  signalRef <- newMVar mempty
  beatRef <- newMVar 0
  bpmRef <- newMVar 120
  let coreConfig = MkCoreConfig { bpmRef, signalRef, beatRef }
  midiBackend <- makeMIDIBackend MkMIDIConfig { midiPortName = "VirMIDI 2-0"}
  oscBackend <- makeOSCBackend MkOSCConfig { portNumber = 57121 }
  let backend = combineBackends midiBackend oscBackend
  _ <- forkIO $ runBackend backend coreConfig
  return coreConfig

main :: IO ()
main = do
  MkCoreConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 160
  modifyMVar_ signalRef $ const . return $ mempty
    & sigMod
    & mapLeftEvent makeNoteEvents

makeNoteEvents :: Event Word8 -> [Event MIDIEvent.Data]
makeNoteEvents MkEvent{interval=(start, dur), payload} =
  [ MkEvent {interval=(start, 0), payload=makeNoteOnData payload}
  , MkEvent {interval=(start + dur, 0), payload=makeNoteOffData payload}
  ]

-- TODO: use lenses
mapLeftEvent :: (Event a -> [Event a']) -> Signal (Either a b) -> Signal (Either a' b)
mapLeftEvent f sig = MkSignal $ \query -> do
  event <- signal sig query
  newEvent <- event & \case
    MkEvent{interval,payload=Left payload} -> f MkEvent{interval,payload} & (fmap . fmap) Left
    MkEvent{interval, payload=Right payload} -> return MkEvent{interval, payload=Right payload}
  return newEvent

splitSig :: forall a b. Signal (Either a b) -> (Signal a, Signal b)
splitSig sig = (leftSig, rightSig)
  where
    leftSig = MkSignal $ \query -> do
      MkEvent{interval, payload=Left payload} <- signal sig query
      return MkEvent{interval, payload}
    rightSig = MkSignal $ \query -> do
      MkEvent{interval, payload=Right payload} <- signal sig query
      return MkEvent{interval, payload}

with :: Functor f => (f a -> a) -> f (a -> a) -> a -> a
with cat mods sig = cat $ ($sig) <$> mods

infixl 4 `tt`

tt :: Rational -> (Signal a -> Signal a) -> Signal a -> Signal a
tt i mod sig = sig
  & slow i
  & mod
  & fast i

fracture :: Int -> (Signal a -> Signal a) -> Signal a -> Signal a
fracture n f = foldr (flip (.)) id ([tt (1/(2^i)) f | i <- [0..n]])

overlay :: (Signal a -> Signal a) -> (Signal a -> Signal a)
overlay f = with mconcat [id, f]

filterSig :: (a -> Bool) -> Signal a -> Signal a
filterSig pred sig = MkSignal $ \query -> signal sig query
  & filter (\MkEvent{payload}-> pred payload)

staccato :: Signal a -> Signal a
staccato sig = sig & (mapInterval . mapDur) (/4)

legato:: Signal a -> Signal a
legato sig = sig & (mapInterval . mapDur) (*1.5)

s :: [Signal a -> Signal a] -> Signal a -> Signal a
s = with switch

ml :: (Functor f, Bifunctor g) => (a -> a) -> f (g a b) -> f (g a b)
ml = fmap . first

mr :: (Functor f, Bifunctor g) => (b -> b) -> f (g a b) -> f (g a b)
mr = fmap . second

sigMod :: Signal (Either Word8 [OSC.OSC]) -> Signal (Either Word8 [OSC.OSC])
sigMod = let (>>) = (flip (.)) in do
  const (embed $ Left 60)
  id
  overlay $ const $ embed $ Right $
    [ OSC.OSC "/fader1" [OSC.OSC_D 1]
    ]
