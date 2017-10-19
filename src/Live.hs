{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
module Live where

import Control.Monad
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

setup :: IO (CoreConfig Word8)
setup = do
  signalRef <- newMVar mempty
  beatRef <- newMVar 0
  bpmRef <- newMVar 120
  let coreConfig = MkCoreConfig { bpmRef, signalRef, beatRef }
  midiBackend <- makeEasyMIDIBackend MkMIDIConfig { midiPortName = "VirMIDI 2-0"}
  -- midiBackend <- makeEasyMIDIBackend MkMIDIConfig { midiPortName = "UM-ONE MIDI 1"}
  _ <- forkIO $ runBackend midiBackend coreConfig
  return coreConfig

makeEasyMIDIBackend :: MIDIConfig -> IO (Backend Word8)
makeEasyMIDIBackend config = do
  midiBackend <- makeMIDIBackend config
  return $ \bpm (beat, beatOffset) clock sig ->
    signal sig (beat, beatOffset)
      & (>>=makeNoteEvents)
      & fmap (makeTimestamp bpm beat clock)
      & midiBackend

main :: IO ()
main = do
  MkCoreConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 160
  modifyMVar_ signalRef $ const . return $ mempty
    & sigMod

makeNoteEvents :: Event Word8 -> [Event MIDIEvent.Data]
makeNoteEvents MkEvent{interval=(start, dur), payload} =
  [ MkEvent {interval=(start, 0), payload=makeNoteOnData payload}
  , MkEvent {interval=(start + dur, 0), payload=makeNoteOffData payload}
  ]

-- TODO: use lenses
mapEvent :: (Event a -> [Event a']) -> Signal a -> Signal a'
mapEvent f sig = MkSignal $ \query -> do
  event <- signal sig query
  newEvent <- event & \case
    MkEvent{interval,payload=payload} -> f MkEvent{interval,payload}
  return newEvent

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

c :: [Signal a -> Signal a] -> Signal a -> Signal a
c = with cat

n :: [Signal a -> Signal a] -> Signal a -> Signal a
n = with nest

ml :: (Functor f, Bifunctor g) => (a -> a) -> f (g a b) -> f (g a b)
ml = fmap . first

mr :: (Functor f, Bifunctor g) => (b -> b) -> f (g a b) -> f (g a b)
mr = fmap . second

rgb :: Float -> Float -> Float -> [OSC.OSC]
rgb r g b =
  [ OSC.OSC "/1/fader1" [OSC.OSC_F r]
  , OSC.OSC "/1/fader2" [OSC.OSC_F g]
  , OSC.OSC "/1/fader3" [OSC.OSC_F b]
  ]

ttsml :: Rational -> [Word8] -> Signal (Either Word8 b) -> Signal (Either Word8 b)
ttsml x fs = tt x $ with switch (fs & fmap (\x -> ml (+x)))

tts :: Rational -> [Signal Word8 -> Signal Word8] -> Signal Word8 -> Signal Word8
tts x fs = tt x $ with switch fs

ttsf :: Rational -> [Word8] -> Signal Word8 -> Signal Word8
ttsf x fs = tt x $ with switch (fs & fmap  (\x -> fmap (+x)))

rep :: Rational ->  Signal a -> Signal a
rep n = tt n $ with switch [id, shift 1 ]


tc :: Rational -> [Signal a -> Signal a] -> Signal a -> Signal a
tc n fs = tt (1/n) $ with cat $ fmap (\f -> tt n f) fs

glisten :: Signal Word8 -> Signal Word8
glisten = fmap $ \x -> (x + ((x `mod` 4) * 12))

sigMod1 :: Signal Word8 -> Signal Word8
sigMod1 = let (>>) = (flip (.)) in do
  const (embed $ 60)
  staccato
  fast 4
  tt (2) $ do
    ttsf 2 [0, 3, 10, 12]
    ttsf (1/8) [17, 12, 10, 12]
    ttsf (1/2) [-12,-24]

sigMod2 :: Signal Word8 -> Signal Word8
sigMod2 = let (>>) = (flip (.)) in do
  const (embed $ 48)
  fmap (+24)
  fast 4
  tc (4) $
    [ ttsf 2 [-27, 0, -1, 7]
    , ttsf 2 [-27, -1, -8, 7]
    ]
  tts (1) [id, id, fmap (+(12)), id]
  tts (1/16) [id, fmap (+(-12))]
  tts (1/32) [id, fmap (+(-2))]


sigMod3 :: Signal Word8 -> Signal Word8
sigMod3 = let (>>) = (flip (.)) in do
  const (embed 60)
  tts 1 [ fmap (+(x)) | x <- [0, 4, 7, 11, 16, 18, 19, 21, 23]]
  fast 6
  tts (2) [ fmap (+(x)) | x <- [0, 0, 0, 0, 0,0, 12]]
  overlay $ do
    const (embed 36)
  --   s [fmap (+0), fmap (+2), fmap (+7), fmap (subtract 5)] & tt (1/16)
  -- fmap (+(-24))

sigMod :: Signal Word8 -> Signal Word8
sigMod = let (>>) = (flip (.)) in do
  const (embed 60)
  n [ fmap (+(x)) | x <- [0, 3, 10, 12, 19, 3]]
  overlay $ do
    const (embed (36))
