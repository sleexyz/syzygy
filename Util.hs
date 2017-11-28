{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad
import Control.Concurrent
import Data.Function ((&))
import Prelude
import Data.String
import qualified Sound.ALSA.Sequencer.Event as MIDIEvent
import qualified Data.HashMap.Strict as HashMap

import Syzygy.Core
import Syzygy.Base
import Syzygy.Signal
import Syzygy.MIDI

setup :: IO CoreConfig
setup = do
  signalRef <- newMVar mempty
  beatRef <- newMVar 0
  bpmRef <- newMVar 160
  let coreConfig = MkCoreConfig { bpmRef, signalRef, beatRef }
  midiDispatcher <- makeMIDIDispatcher MkMIDIConfig { midiPortName = "VirMIDI 2-0"}
  _ <- forkIO $ runDispatcher (midiDispatcher) coreConfig
  return coreConfig

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

pmap :: (Int -> Int) -> (Signal ParamMap -> Signal ParamMap)
pmap = fmap . ((flip HashMap.adjust  "pitch") . mapVI)

smap :: (Double -> Double) -> (Signal ParamMap -> Signal ParamMap)
smap = fmap . ((flip HashMap.adjust  "speed") . mapVF)

vmap :: (Int -> Int) -> (Signal ParamMap -> Signal ParamMap)
vmap = fmap . (flip HashMap.adjust  "value") . mapVI

nope :: (a -> a) -> (a -> a)
nope _ y = y

palindrome :: [a] -> [a]
palindrome x = x ++ reverse x

t :: [Signal a -> Signal a] -> Rational -> Signal a -> Signal a
t xs n = s (fmap (& tt (n)) xs) & tt (recip n)

m :: Monoid a => a
m = mempty

i :: a -> a
i = id
