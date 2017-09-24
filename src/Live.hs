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
  -- let midiPortName = "UM-ONE MIDI 1"
  let midiPortName = "VirMIDI 2-0"
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

sigMod :: Signal Word8 -> Signal Word8
sigMod = let (>>) = (flip (.)) in do
  const (embed 24)
  with nest [ fmap (+x)| x <- [0, 12, 19, 0]]
  tt (1/2) $ with switch [fmap (+2) , id]
  fast 1
  tt (1/4) $ with switch [fmap (+29), fmap (+24)]
  overlay $ do
    tt 4 $ with switch
      [ fmap (+0)
      , fmap (+24)
      , fmap (subtract 7)
      ]
    tt 8 $ with switch
      [ fmap (+0)
      , fmap (subtract 7)
      , fmap (subtract 24)
      ]
    overlay $ shift (0.5)
  tt (1/8) $ with switch [id, (fmap (subtract 7))]
