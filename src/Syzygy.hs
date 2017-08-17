{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Syzygy where

import Data.Profunctor (lmap, rmap)
import Data.Function ((&))
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Control.Concurrent.MVar
import qualified Vivid.OSC as OSC
import qualified Data.Time as Time


type Time = Rational

type Interval = (Time, Time)

data Event a = MkEvent
  { interval :: (Time, Time)
  , payload :: a
  } deriving (Eq, Show, Functor)

-- | A signal is defined by the "integral" of a sampling function
newtype Signal a = MkSignal { signal :: Interval -> [Event a] }
  deriving (Functor, Monoid)

embed :: a -> Signal a
embed x = MkSignal $ \(queryStart, queryEnd) -> do
  let
    start = (fromIntegral @Integer) . floor $ queryStart
    end = (fromIntegral @Integer) . ceiling $ queryEnd
  beat <- [start..end - 1]
  return MkEvent { interval = (beat, (beat + 1)), payload = x }

pruneSignal :: Signal a -> Signal a
pruneSignal (MkSignal sig) = MkSignal $ \(queryStart, queryEnd) ->
  let
    inBounds MkEvent {interval = (start, _)} = start >= queryStart && start < queryEnd
  in
    filter inBounds $ sig (queryStart, queryEnd)

-- | shift forward in time
shift :: Time -> Signal a -> Signal a
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

-- | interleave within one period
interleave :: [Signal a] -> Signal a
interleave sigs = MkSignal $ \query -> do
  let (fromIntegral -> len) = length sigs
  (sig, n) <- zip sigs [0..]
  signal (shift (n/len) sig) query

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
  { action :: Rational -> IO ()
  , superDirtSocket :: Network.Socket
  , clockRef :: MVar Rational
  , signalRef :: MVar (Signal BS.ByteString)
  }

makeLocalUDPConnection :: Network.PortNumber -> IO Network.Socket
makeLocalUDPConnection portNumber = do
  (a:_) <- Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNumber))
  superDirtSocket <- Network.socket (Network.addrFamily a) Network.Datagram Network.defaultProtocol
  Network.connect superDirtSocket (Network.addrAddress a)
  return superDirtSocket

makeEnv :: Network.PortNumber -> IO Env
makeEnv portNumber = do
  superDirtSocket <- makeLocalUDPConnection portNumber
  clockRef <- newMVar (0 :: Rational)
  signalRef <- newMVar (mempty :: Signal BS.ByteString)
  let
    action :: Rational -> IO ()
    action = makeAction env
    env = MkEnv { superDirtSocket, clockRef, signalRef, action }
  return env

makeAction :: Env -> Rational -> IO ()
makeAction MkEnv{superDirtSocket, clockRef, signalRef} cps = do
  modifyMVar_ clockRef (return . (+1))
  now <- Time.getCurrentTime
  signal <- readMVar signalRef
  let oscEvents = querySignal now cps (0, 1) signal
  traverse (NetworkBS.send superDirtSocket . toOSCBundleTest) oscEvents
  threadDelay (floor $ recip cps * 1000000)

main :: IO ()
main = do
  putStrLn "Hello"
  -- withMockSuperDirtServer handleBundle $ \port -> do
  --   putStrLn "Hello"
  --   MkEnv {superDirtSocket, clockRef} <- makeEnv port
  --   forever $ do
  --     threadDelay (floor $ recip 1 * 1000000)
  --     now <- Time.getCurrentTime
  --     let
  --       signal = stack
  --         [ fast 2 $ interleave [embed "sn", embed "bd", embed "bd"]
  --         , fast 3 $ interleave [fast 1 $ embed "dr55", embed "bd"]
  --         ]
  --       oscEvents = querySignal now 1 (0, 1) signal
  --       sendEvent = NetworkBS.send superDirtSocket . toOSCBundleTest
  --     traverse sendEvent oscEvents
  --     return ()

-- makeBundle :: Time.UTCTime -> Signal () -> [OSC.OSCBundle]
 --   foo :: B.ByteString
 --   foo = OSC.encodeOSC $ OSC.OSC "/play2" [OSC.OSC_S "cps", OSC.OSC_I 1, OSC.OSC_S "s", OSC.OSC_S "bd"]
 -- print $ B.toLazyByteString $ B.byteStringHex foo
