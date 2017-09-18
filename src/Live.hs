{-# LANGUAGE ParallelListComp #-}
module Live where

import Control.Concurrent
import Data.Word (Word8)
import Data.Function ((&))
import qualified Data.Vector.Unboxed as V
import System.Random.MWC (uniformVector, create)
import System.IO.Unsafe (unsafePerformIO)

import Syzygy.Core
import Syzygy.Signal
import Syzygy.MIDI

-- | Lookup table of random bytes of length 4096
randomBits :: V.Vector Word8
randomBits = unsafePerformIO $ do
  gen <- create
  vec <- uniformVector gen 4096
  return vec

setup :: IO MIDIConfig
setup = do
  signalRef <- newMVar mempty
  clockRef <- newMVar 0
  bpmRef <- newMVar 120
  let midiPortName = "VirMIDI 2-0"
  let config = MkMIDIConfig { bpmRef, midiPortName, signalRef, clockRef}
  _ <- forkIO $ runBackend backend config
  return config

main :: IO ()
main = do
  MkMIDIConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 60
  modifyMVar_ signalRef $ const . return $ sig

randByte :: Signal Word8
randByte = MkSignal $ \query@(queryStart, _) ->
  let
    val = randomBits `V.unsafeIndex` (floor queryStart `mod` 4096)
  in
    signal (embed val) query

with :: Functor f => (f a -> a) -> f (a -> a) -> a -> a
with cat mods sig = cat $ ($sig) <$> mods

tt :: Rational -> (Signal a -> Signal a) -> Signal a -> Signal a
tt i mod sig = sig
  & slow i
  & mod
  & fast i

sig :: Signal Word8
sig = embed 0
  & with mconcat
    [ id
    , fast 5 . with seive
      [ fmap (+12)
      , fmap (+10)
      , fmap (+3)
      ]
    ]
  & fmap (+60)
