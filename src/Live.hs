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
import qualified Data.HashMap.Strict as HashMap
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
  midiDispatcher <- makeMIDIDispatcher MkMIDIConfig { midiPortName = "VirMIDI 2-0"}
  -- oscDispatcher <- makeOSCDispatcher MkOSCConfig { portNumber = 57120}
  -- _ <- forkIO $ runDispatcher (midiDispatcher `composeDispatcher` oscDispatcher) coreConfig
  _ <- forkIO $ runDispatcher (midiDispatcher) coreConfig
  return coreConfig

main :: IO ()
main = do
  MkCoreConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 160
  modifyMVar_ signalRef $ const . return $ mempty
    & sigMod

composeDispatcher :: Dispatcher -> Dispatcher -> Dispatcher
composeDispatcher b1 b2 env = do
  b1 env
  b2 env

makeMIDIDispatcher :: MIDIConfig -> IO Dispatcher
makeMIDIDispatcher config = do
  sendMIDIEvents <- makeMIDITimestampedEventDispatcher config
  return $ \MkEnv{bpm, interval=(beat, _),clock, events} -> do
    events
      & (>>=makeMIDIEvents)
      & fmap (makeTimestamp bpm beat clock)
      & sendMIDIEvents

makeMIDIEvents :: Event ParamMap -> [Event MIDIEvent.Data]
makeMIDIEvents MkEvent{interval=(start, dur), payload}
  | Just (VI (fromIntegral -> channel)) <- HashMap.lookup "channel" payload
  , Just (VI (fromIntegral -> pitch)) <- HashMap.lookup "pitch" payload
  = [ MkEvent {interval=(start, 0), payload=makeNoteOnData channel pitch}
    , MkEvent {interval=(start + dur, 0), payload=makeNoteOffData channel pitch}
    ]
makeMIDIEvents MkEvent{interval=(start,_), payload}
  | Just (VI (fromIntegral -> param)) <- HashMap.lookup "param" payload
  , Just (VI (fromIntegral -> value)) <- HashMap.lookup "value" payload
  = return $ MkEvent {interval=(start, 0), payload=makeCtrlMessage param value}
makeMIDIEvents MkEvent{} = []

makeOSCDispatcher :: OSCConfig -> IO Dispatcher
makeOSCDispatcher config = do
  sendOSCEvents <- makeOSCTimestampedEventDispatcher config
  return $ \MkEnv{bpm, interval=(beat, _),clock, events} -> do
    events
      & (>>=makeOSCEvents)
      & fmap (makeTimestamp bpm beat clock)
      & sendOSCEvents

makeOSCEvents :: Event ParamMap -> [Event [OSC.OSC]]
makeOSCEvents event@MkEvent{payload}
  | Just (VS sound) <- HashMap.lookup "s" payload
  = let
      values :: [OSC.OSCDatum]
      values = mconcat ([ [OSC.OSC_S "s", OSC.OSC_S sound]] ++ extractDirtParameters payload)
    in
      return $ event { payload = [OSC.OSC "/play2" values] }
makeOSCEvents MkEvent{} = []

customParams :: [(BS.ByteString, Value)]
customParams =
  [ ("speed", VF 1)
  , ("gain", VF 1)
  , ("pan", VF 0.5)
  ]

extractDirtParameters :: ParamMap -> [[OSC.OSCDatum]]
extractDirtParameters payload = do
  (key, _) <- customParams
  return $ lookupF key payload
  where
      lookupF :: BS.ByteString -> ParamMap -> [OSC.OSCDatum]
      lookupF key map = case HashMap.lookup key map of
        Just (VF x) -> [OSC.OSC_S key, OSC.OSC_F (realToFrac x)]
        _ -> []

dirt :: BS.ByteString ->  Signal ParamMap -> Signal ParamMap
dirt s = const . embed $ HashMap.fromList $ [ ("s", VS s)] ++ customParams

note :: Int -> Int -> Signal ParamMap -> Signal ParamMap
note c p = const . embed $ HashMap.fromList [("channel", VI c), ("pitch", VI p)]

ctrl :: Int -> Int -> Signal ParamMap -> Signal ParamMap
ctrl c v = const . embed $ HashMap.fromList [("param", VI c), ("value", VI v)]

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
staccato sig = sig & (mapInterval . mapDur) (/2)

legato:: Signal a -> Signal a
legato sig = sig & (mapInterval . mapDur) (*2)

s :: [Signal a -> Signal a] -> Signal a -> Signal a
s = with switch

c :: [Signal a -> Signal a] -> Signal a -> Signal a
c = with cat

n :: [Signal a -> Signal a] -> Signal a -> Signal a
n = with nest

-- m :: [Signal a -> Signal a] -> Signal a -> Signal a
-- m = with mconcat

pmap :: (Int -> Int) -> (Signal ParamMap -> Signal ParamMap)
pmap = fmap . ((flip HashMap.adjust  "pitch") . mapVI)

smap :: (Double -> Double) -> (Signal ParamMap -> Signal ParamMap)
smap = fmap . ((flip HashMap.adjust  "speed") . mapVF)

vmap :: (Int -> Int) -> (Signal ParamMap -> Signal ParamMap)
vmap = fmap . (flip HashMap.adjust  "value") . mapVI

nope :: (a -> a) -> (a -> a)
nope _ y = y

sigMod_glidey :: Signal ParamMap -> Signal ParamMap
sigMod_glidey = let (>>) = (flip (.)) in do
  overlay $ do
    note 0 (60)

  overlay $ do
    ctrl 1 60
    tt (8) $ s [vmap (+(x*12)) | x <- [0..7]]
    fast 1
    s [vmap (+24), vmap (+12), id] & tt (1/16)


  overlay $ do
    dirt "clubkick"
    s [smap (*(2**((x)/12))) | x <- [12, 24, 60]]
    fast 4
    s [smap (/2), id] & tt 1

  overlay $ do
    note 2 (30)

  s [id, id, shift 0.5, id]

sigMod_morning :: Signal ParamMap -> Signal ParamMap
sigMod_morning = let (>>) = (flip (.)) in do
  overlay $ do
    note 0 (60)
    fast 2
    s [pmap (+(0)), pmap (+12)]

  overlay $ do
    ctrl 1 60
    tt (1) $ s [vmap (+(x*12)) | x <- [0..3]]
    s [vmap (+12), vmap (+12)] & tt (1/16)


  overlay $ do
    dirt "click"
    s [smap (*(2**((x)/12))) | x <- [12, 24, 60]]
    fast 2
    s [smap (/2), id] & tt 1
    shift 2.5

  overlay $ do
    note 2 (30)
    fast 2

  overlay $ do
    note 1 (30)
    fast 4

  s [id, shift 0.5, shift 0.5 . pmap (subtract 12) . smap (+24), id] & tt 2
  -- pmap (+(-12))
  -- vmap (+(-24))

sinr :: Integral a => a -> a -> a -> a
sinr x (fromIntegral -> min) (fromIntegral -> max) = floor $ amp * sin (2 * pi * fromIntegral x / 360) + avg
  where
    amp = (max - min) / 2
    avg = (max + min) / 2

sigMod_morning2 :: Signal ParamMap -> Signal ParamMap
sigMod_morning2 = let (>>) = (flip (.)) in do
  overlay $ do
    note 0 (60)
    -- s [fast 1, fast 2] & tt (1)
    s [id, pmap (+12)] & tt (1/32)
    -- s [id, shift 0.5] & tt (1/6)
    -- slow 1.5


  overlay $ do
    ctrl 0 120
    -- ctrl 0 200
    s [vmap (+0), vmap (+4)] & tt (1/8)
    fast 127

  overlay $ do
    note 1 (0)
    shift 0.5

  overlay $ do
    note 2 (0)
  -- s [id, shift 0.5, shift 0.5, id] & tt 2
  -- s [id, shift 0.25 . rep 4 ] & tt 1

m :: Monoid a => a
m = mempty

i :: a -> a
i = id

sigMod_morning3 :: Signal ParamMap -> Signal ParamMap
sigMod_morning3 = let (>>) = (flip (.)) in do
  nope $ overlay $ do
    note 3 (60)
    legato
    legato
    pmap (+24)
    s [fast 2, pmap (subtract 12), shift 0.5, i] & tt (1)
    -- s [slow 1.5, i]

    overlay $ do
      s [ctrl 0 120, ctrl 0 20] & tt (1)
      s [vmap (+x) | x <- [0..3]] & tt (1/4)

    -- s [i, i, shift 0.5, i] & tt (1)

  nope $ overlay $ do
    note 2 (60)
    s [pmap (+x) | x <- [0, 5]]
    s [pmap (+x) | x <- [0, 0, 12]] & tt (1/2)
    s [pmap (+x) | x <- [0,-2]] & tt (1.5)
    slow 2
    overlay $ do
      ctrl 0 95
      s [vmap (+x) | x <- [0..3]]
      fast 8

  -- s [pmap (+(-2)), i] & tt (1/32)

  -- overlay $ do
  --   note 0 (0)
  --   shift 0.5

  --   overlay $ do
  --     note 1 (0)


  -- s [fast 1, fast 2 . shift 0.5]

  -- s [rep 1.5, id] & tt (1)
  -- s [fast 1, fast 2] & tt (1)
  -- s [fast 2, fast 1] & tt (1/2)
  -- s [shift 0.5, id] & tt (1/4)

  -- s [id, shift 0.5, shift 0.5, id] & tt (2)
  -- s [id, rep 2 . shift 0.5 . rep 2, i, rep 2 ] & tt 1

sigMod_morning4 :: Signal ParamMap -> Signal ParamMap
sigMod_morning4 = let (>>) = (flip (.)) in do
  i $ overlay $ do
    note 3 (60)
    staccato
    pmap (+24)

    overlay $ do
      s [ctrl 0 120, ctrl 0 20] & tt (1)
      s [vmap (+x) | x <- [0..3]] & tt (1/4)

    c [fast 2, c [fast 4, c [fast 4, i]]]
    s [id, pmap (+(-24))] & tt (1)
    -- s [slow 1.5]
    -- s [i, i, shift 0.5, i] & tt (1)

  nope $ overlay $ do
    note 2 (60)
    staccato
    staccato
    fast 2
    c [c [i, c [i, fast 2, i]], c[i, i, fast 2]]
    pmap (+12)
    overlay $ do
      pmap (+12)
      shift 0.25
    overlay $ do
      ctrl 0 101
      s [vmap (+x) | x <- [0..3]] & tt (1/4)


  -- s [pmap (+(-2)), i] & tt (1/32)

  overlay $ do
    note 0 (0)
    shift 0.5

    overlay $ do
      note 1 (0)


  -- s [fast 1, fast 2 . shift 0.5]

  -- s [rep 1.5, id] & tt (1)
  -- s [fast 1, fast 2] & tt (1)
  -- s [fast 2, fast 1] & tt (1/2)
  -- s [shift 0.5, id] & tt (1/4)

  -- s [id, shift 0.5, shift 0.5, id] & tt (2)
  -- s [id, rep 2 . shift 0.5 . rep 2, i, rep 2 ] & tt 1

sigMod :: Signal ParamMap -> Signal ParamMap
sigMod = let (>>) = (flip (.)) in do
  i $ overlay $ do
    note 3 (60)
    staccato
    pmap (+24)

    overlay $ do
      s [ctrl 0 120, ctrl 0 20] & tt (1)
      s [vmap (+x) | x <- [0..3]] & tt (1/4)

    c [fast 2, c [fast 4, c [fast 4, i]]]
    s [id, pmap (+(-24))] & tt (1)
    -- s [slow 1.5]
    -- s [i, i, shift 0.5, i] & tt (1)

  nope $ overlay $ do
    note 2 (60)
    staccato
    staccato
    fast 2
    c [c [i, c [i, fast 2, i]], c[i, i, fast 2]]
    pmap (+12)
    overlay $ do
      pmap (+12)
      shift 0.25
    overlay $ do
      ctrl 0 101
      s [vmap (+x) | x <- [0..3]] & tt (1/4)


  -- s [pmap (+(-2)), i] & tt (1/32)

  overlay $ do
    note 0 (0)
    shift 0.5

    overlay $ do
      note 1 (0)


  -- s [fast 1, fast 2 . shift 0.5]

  -- s [rep 1.5, id] & tt (1)
  -- s [fast 1, fast 2] & tt (1)
  -- s [fast 2, fast 1] & tt (1/2)
  -- s [shift 0.5, id] & tt (1/4)

  -- s [id, shift 0.5, shift 0.5, id] & tt (2)
  -- s [id, rep 2 . shift 0.5 . rep 2, i, rep 2 ] & tt 1
