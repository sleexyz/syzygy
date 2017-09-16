module Live where

import Control.Concurrent
import Data.Word (Word8)

import Syzygy.Core
import Syzygy.Signal
import Syzygy.MIDI

main :: IO ()
main = do
  MkMIDIConfig {signalRef} <- runOnce $ do
    signalRef <- newMVar mempty
    clockRef <- newMVar 0
    bpmRef <- newMVar 120
    let midiPortName = "VirMIDI 2-0"
    let config = MkMIDIConfig { bpmRef, midiPortName, signalRef, clockRef}
    _ <- forkIO $ runBackend backend config
    return config
  modifyMVar_ signalRef $ const . return $ sig

sig :: Signal Word8
sig = nest $ (embed$) <$> [x + 40 | x <- [0, 12, 28, 24, 16]]
