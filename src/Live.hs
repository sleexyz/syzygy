module Live where

import Control.Concurrent
import Data.Word (Word8)
import Data.Function ((&))

import Syzygy.Core
import Syzygy.Signal
import Syzygy.MIDI

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
  modifyMVar_ bpmRef $ const . return $ 120
  modifyMVar_ signalRef $ const . return $ sig

sig :: Signal Word8
sig = mconcat
  [ fast (4/16) $ nest $ (embed$) <$> [x + 52 + y | x <- [0, 12, 24, 28, 16], y <- [0, 5, 12, -7, -12]]
  , nest [embed 32]
  ]
  & fast (1/2)
  & fmap (+(0))
