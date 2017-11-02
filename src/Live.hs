{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PatternGuards #-}
module Live where

import Control.Monad
import Control.Concurrent
import Data.Function ((&))
import Prelude
import Data.String
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent
import qualified Vivid.OSC as OSC
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

import Syzygy.Core
import Syzygy.Base
import Syzygy.Signal
import Syzygy.MIDI
import Syzygy.OSC

setup :: IO CoreConfig
setup = do
  signalRef <- newMVar mempty
  beatRef <- newMVar 0
  bpmRef <- newMVar 120
  let coreConfig = MkCoreConfig { bpmRef, signalRef, beatRef }
  let midiConfig = MkMIDIConfig { midiPortName = "VirMIDI 2-0"}
  let oscConfig = MkOSCConfig { portNumber = 57120}
  backend <- makeBackend midiConfig oscConfig
  _ <- forkIO $ runBackend backend coreConfig
  return coreConfig

main :: IO ()
main = do
  MkCoreConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 160
  modifyMVar_ signalRef $ const . return $ mempty
    & sigMod

composeBackend :: Backend -> Backend -> Backend
composeBackend b1 b2 env = do
  b1 env
  b2 env

makeBackend :: MIDIConfig -> OSCConfig -> IO Backend
makeBackend midiConfig oscConfig = do
  midiBackend <- makeMIDIBackend midiConfig
  oscBackend <- makeOSCBackend oscConfig
  return $ midiBackend `composeBackend` oscBackend

makeMIDIBackend :: MIDIConfig -> IO Backend
makeMIDIBackend config = do
  sendMIDIEvents <- makeMIDISendTimestampedEvents config
  return $ \MkEnv{bpm, interval=(beat, _),clock, events} -> do
    events
      & (>>=makeMIDIEvents)
      & fmap (makeTimestamp bpm beat clock)
      & sendMIDIEvents

makeMIDIEvents :: Event ParamMap -> [Event MIDIEvent.Data]
makeMIDIEvents MkEvent{interval=(start, dur), payload}
  | Just (VI (fromIntegral -> channel)) <- Map.lookup "channel" payload
  , Just (VI (fromIntegral -> pitch)) <- Map.lookup "pitch" payload
  = [ MkEvent {interval=(start, 0), payload=makeNoteOnData channel pitch}
    , MkEvent {interval=(start + dur, 0), payload=makeNoteOffData channel pitch}
    ]
makeMIDIEvents MkEvent{interval=(start,_), payload}
  | Just (VI (fromIntegral -> param)) <- Map.lookup "param" payload
  , Just (VI (fromIntegral -> value)) <- Map.lookup "value" payload
  = return $ MkEvent {interval=(start, 0), payload=makeCtrlMessage param value}
makeMIDIEvents MkEvent{} = []

makeOSCBackend :: OSCConfig -> IO Backend
makeOSCBackend config = do
  sendOSCEvents <- makeOSCSendTimestampedEvents config
  return $ \MkEnv{bpm, interval=(beat, _),clock, events} -> do
    events
      & (>>=makeOSCEvents)
      & fmap (makeTimestamp bpm beat clock)
      & sendOSCEvents

makeOSCEvents :: Event ParamMap -> [Event [OSC.OSC]]
makeOSCEvents event@MkEvent{payload}
  | Just (VS sound) <- Map.lookup "s" payload
  = let
      values :: [OSC.OSCDatum]
      values = mconcat
        [ [OSC.OSC_S "s", OSC.OSC_S sound]
        , lookupF "speed" payload
        ]
      lookupF :: (forall s. IsString s => s) -> ParamMap -> [OSC.OSCDatum]
      lookupF key map = case Map.lookup key map of
        Just (VF x) -> [OSC.OSC_S key, OSC.OSC_F (realToFrac x)]
        _ -> []
    in
      return $ event { payload= [OSC.OSC "/play2" values] }
makeOSCEvents MkEvent{} = []

sound :: BS.ByteString ->  Signal ParamMap -> Signal ParamMap
sound s = const . embed $ Map.fromList
  [ ("s", VS s)
  , ("speed", VF 1)
  ]

note :: Int -> Int -> Signal ParamMap -> Signal ParamMap
note c p = const . embed $ Map.fromList [("channel", VI c), ("pitch", VI p)]

ctrl :: Int -> Int -> Signal ParamMap -> Signal ParamMap
ctrl c v = const . embed $ Map.fromList [("param", VI c), ("value", VI v)]

with :: Functor f => (f a -> a) -> f (a -> a) -> a -> a
with cat mods sig = cat $ ($sig) <$> mods

tt :: Rational -> (Signal a -> Signal a) -> Signal a -> Signal a
tt i mod sig = sig
  & slow i
  & mod
  & fast i

fracture :: Int -> (Signal a -> Signal a) -> Signal a -> Signal a
fracture n f = foldr (flip (.)) id ([tt (1/(2^i)) f | i <- [0..n]])

overlay :: (Signal a -> Signal a) -> (Signal a -> Signal a)
overlay f = with mconcat [id, f]

rep :: Rational ->  Signal a -> Signal a
rep n = tt n $ with switch [id, shift 1 ]

staccato :: Signal a -> Signal a
staccato sig = sig & (mapInterval . mapDur) (/4)

legato:: Signal a -> Signal a
legato sig = sig & (mapInterval . mapDur) (*4)

s :: [Signal a -> Signal a] -> Signal a -> Signal a
s = with switch

c :: [Signal a -> Signal a] -> Signal a -> Signal a
c = with cat

n :: [Signal a -> Signal a] -> Signal a -> Signal a
n = with nest

m :: [Signal a -> Signal a] -> Signal a -> Signal a
m = with mconcat

pmap :: (Int -> Int) -> (Signal ParamMap -> Signal ParamMap)
pmap = fmap . ((flip Map.adjust  "pitch") . mapVI)

smap :: (Double -> Double) -> (Signal ParamMap -> Signal ParamMap)
smap = fmap . ((flip Map.adjust  "speed") . mapVF)

vmap :: (Int -> Int) -> (Signal ParamMap -> Signal ParamMap)
vmap = fmap . (flip Map.adjust  "value") . mapVI

sigMod :: Signal ParamMap -> Signal ParamMap
sigMod = let (>>) = (flip (.)) in do
  id
  id
  overlay $ do
    note 1 (30)
    fast 4

  overlay $ do
    ctrl 1 30
    tt (1) $ s [vmap (+(x*12)) | x <- [0..7]]
    fast 8

  overlay $ do
    sound "dr55"
    s [smap (*x) | x <- [1..4]]
    s [ fast 2 . smap (/1.5), id]

  s [id, shift 0.5]

  s [pmap (+x) | x <- [0, 30]] & tt (1/16)

  overlay $ do
    note 2 (30)

  s [pmap (const 127), id]

  tt (1/16) $ s [id, vmap (+12)]
