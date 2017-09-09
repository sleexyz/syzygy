{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Syzygy where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Function ((&))
import Data.Profunctor (lmap, rmap)

import qualified Data.ByteString as BS
import qualified Data.Time as Time
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import qualified Vivid.OSC as OSC

type Interval = (Rational, Rational)

data Event a = MkEvent
  { interval :: (Rational, Rational)
  , payload :: a
  } deriving (Eq, Show, Functor)

newtype Signal a = MkSignal { signal :: Interval -> [Event a] }
  deriving (Functor, Monoid)

embed :: a -> Signal a
embed x = MkSignal $ \(queryStart, queryEnd) -> do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return MkEvent { interval = (beat, beat + 1), payload = x }

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, queryEnd) ->
  let
    inBounds MkEvent {interval = (start, _)} = start >= queryStart && start < queryEnd
  in
    filter inBounds $ sig (queryStart, queryEnd)

-- | shift forward in time
shift :: Rational -> Signal a -> Signal a
shift t MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> (start - t, end - t ))
      & rmap (fmap $ \ev@MkEvent { interval = (start, end) } -> ev { interval = (start + t, end + t) })

-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> ( start * n, end * n ))
      & rmap (fmap $ \ev@MkEvent { interval = (start, end) } -> ev { interval = (start / n, end / n) })

-- | stack in parallel
stack :: [Signal a] -> Signal a
stack sigs = MkSignal $ \query -> do
  MkSignal{signal} <- sigs
  signal query


-- | filter a signal by a predicate on events
_filterSignal :: (Event a -> Bool) -> Signal a -> Signal a
_filterSignal predicate sig = MkSignal $ \query -> filter predicate $ signal sig query

-- | interleave signals within a single cycle
interleave :: [Signal a] -> Signal a
interleave sigs = stack $ filterAndShift <$> zip sigs [0..]
  where
    n :: Rational
    n = fromIntegral $ length sigs

    makeSieve :: Rational -> Event a -> Bool
    makeSieve i MkEvent { interval = (start, _) } =
      let
        startFract = snd $ properFraction @ Rational @ Integer start
      in
        startFract >= (i/ n) && startFract < ((i + 1) / n)

    filterAndShift:: (Signal a, Rational) -> Signal a
    filterAndShift (sig, i) = sig
      & shift (i/n)
      & _filterSignal (makeSieve i)

-- | interleaves scaled signals within a single cycle
nest :: [Signal a] -> Signal a
nest sigs = interleave $ fast n <$> sigs
  where
    n :: Rational
    n = fromIntegral $ length sigs


-- | Query a signal for once cycle at the given rate, relative to some absolute time
querySignal :: forall a. Time.UTCTime -> Rational -> Interval -> Signal a -> [(Time.UTCTime, a)]
querySignal now cps query sig = fmap formatEvent events
  where
    queryStart :: Rational
    (queryStart, _) = query

    events :: [Event a]
    events = signal (pruneSignal sig) query

    formatEvent :: Event a -> (Time.UTCTime, a)
    formatEvent MkEvent{interval=(start, _), payload} =
      let
        delay = (start - queryStart)  * (recip cps)
        timestamp = Time.addUTCTime (fromRational delay) now
      in
        (timestamp, payload)

toOSCBundleTest :: (Time.UTCTime, BS.ByteString) -> BS.ByteString
toOSCBundleTest (time, sound) = OSC.encodeOSCBundle $ OSC.OSCBundle timestamp [Right $ OSC.OSC "/play2" message]
  where
    timestamp :: OSC.Timestamp
    timestamp = OSC.utcToTimestamp time

    message :: [OSC.OSCDatum]
    message = [OSC.OSC_S "s", OSC.OSC_S sound]

data Env = MkEnv
  { sendEvents :: IO ()
  , superDirtSocket :: Network.Socket
  , clockRef :: MVar Rational
  , signalRef :: MVar (Signal BS.ByteString)
  }

data Config = MkConfig
  { portNumber :: Network.PortNumber
  , cps :: Rational
  }

makeEnv :: Config -> IO Env
makeEnv config@MkConfig{portNumber } = do
  superDirtSocket <- _makeLocalUDPConnection portNumber
  clockRef <- newMVar (0 :: Rational)
  signalRef <- newMVar (mempty :: Signal BS.ByteString)
  let
    sendEvents :: IO ()
    sendEvents = _makeSendEvents config env

    env = MkEnv { superDirtSocket, clockRef, signalRef, sendEvents }
  return env

_makeLocalUDPConnection :: Network.PortNumber -> IO Network.Socket
_makeLocalUDPConnection portNumber = do
  (a:_) <- Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNumber))
  superDirtSocket <- Network.socket (Network.addrFamily a) Network.Datagram Network.defaultProtocol
  Network.connect superDirtSocket (Network.addrAddress a)
  return superDirtSocket

_makeSendEvents :: Config -> Env -> IO ()
_makeSendEvents MkConfig{cps} MkEnv{superDirtSocket, clockRef, signalRef } = do
  now <- Time.getCurrentTime
  signal <- readMVar signalRef
  clockVal <- modifyMVar clockRef (\x -> return (x + 1, x))
  let oscEvents = querySignal now cps (clockVal, clockVal + 1) signal
  _ <- traverse (NetworkBS.send superDirtSocket . toOSCBundleTest) oscEvents
  return ()

delayOneCycle :: Rational -> IO ()
delayOneCycle cps = threadDelay (floor $ recip cps * 1000000)

main :: IO ()
main = do
  let portNumber = 57120
  let cps = 1
  MkEnv{signalRef, sendEvents} <- makeEnv MkConfig {cps, portNumber}
  modifyMVar_ signalRef (const . return $ embed "bd")
  forever $ do
    sendEvents
    delayOneCycle cps
