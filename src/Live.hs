{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RebindableSyntax #-}
module Live where

import Control.Concurrent
import Data.Word (Word8)
import Data.Function ((&))
import Prelude
import Data.String

import Syzygy.Core
import Syzygy.Signal
import Syzygy.MIDI

setup :: IO MIDIConfig
setup = do
  signalRef <- newMVar mempty
  clockRef <- newMVar 0
  bpmRef <- newMVar 120
  let midiPortName = "UM-ONE MIDI 1"
  -- let midiPortName = "VirMIDI 2-0"
  let config = MkMIDIConfig { bpmRef, midiPortName, signalRef, clockRef}
  _ <- forkIO $ runBackend backend config
  return config

main :: IO ()
main = do
  MkMIDIConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 120
  modifyMVar_ signalRef $ const . return $ sigMod mempty

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

lpf :: Word8 -> Signal Word8 -> Signal Word8
lpf i = filterSig $ (<i)

hpf :: Word8 -> Signal Word8 -> Signal Word8
hpf i = filterSig $ (>i)

staccato :: Signal a -> Signal a
staccato sig = sig & (mapInterval . mapDur) (/4)

sigMod :: Signal Word8 -> Signal Word8
sigMod = let (>>) = (flip (.)) in do
  staccato
  const (embed 60)
  with switch [ fmap (+(x + y)) | x <- [0, 3, 7, 10, 15, 17, 22, 24, 26, 27] >>= replicate 1 | y <- cycle [0, -24, 12]]
  fast 16
  tt (1/2) $ with switch [id, fmap (subtract 12)]
  tt (1/8) $ with switch [id, fmap (subtract 2)]
