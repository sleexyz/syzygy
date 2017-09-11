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
      & (rmap . fmap) (\ev@MkEvent { interval = (start, end) } -> ev { interval = (start + t, end + t) })

-- | scale faster in time
fast :: Rational -> Signal a -> Signal a
fast n MkSignal {signal=originalSignal} = MkSignal {signal}
  where
    signal = originalSignal
      & lmap (\(start, end) -> ( start * n, end * n ))
      & (rmap . fmap) (\ev@MkEvent { interval = (start, end) } -> ev { interval = (start / n, end / n) })

-- | filter a signal by a predicate on events
_filterSignal :: (Event a -> Bool) -> Signal a -> Signal a
_filterSignal predicate sig = MkSignal $ \query -> filter predicate $ signal sig query

-- | interleave signals within a single cycle
interleave :: [Signal a] -> Signal a
interleave sigs = mconcat $ filterAndShift <$> zip sigs [0..]
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

toOSCBundleTest :: (Time.UTCTime, BS.ByteString) -> BS.ByteString
toOSCBundleTest (time, sound) = OSC.encodeOSCBundle $ OSC.OSCBundle timestamp [Right $ OSC.OSC "/play2" message]
  where
    timestamp :: OSC.Timestamp
    timestamp = OSC.utcToTimestamp time

    message :: [OSC.OSCDatum]
    message = [OSC.OSC_S "s", OSC.OSC_S sound]

data Env = MkEnv
  { sendEvents :: Rational -> [ Event BS.ByteString ] -> IO ()
  , superDirtSocket :: Network.Socket
  , clockRef :: MVar Rational
  , signalRef :: MVar (Signal BS.ByteString)
  }

data Config = MkConfig
  { superDirtPortNumber :: Network.PortNumber
  , bpm :: Int
  , tpb :: Int
  }

formatEvent :: Time.UTCTime -> Rational -> Int -> Event a -> (Time.UTCTime, a)
formatEvent now clockVal bpm MkEvent{interval=(eventStart, _), payload} =
  let
    delay = (eventStart - clockVal)  * (60 / fromIntegral bpm)
    timestamp = Time.addUTCTime (fromRational delay) now
  in
    (timestamp, payload)

makeEnv :: Config -> IO Env
makeEnv MkConfig{superDirtPortNumber, bpm } = do
  superDirtSocket <- _makeLocalUDPConnection superDirtPortNumber
  clockRef <- newMVar (0 :: Rational)
  signalRef <- newMVar (mempty :: Signal BS.ByteString)
  let
    sendEvents :: Rational -> [ Event BS.ByteString ] -> IO ()
    sendEvents clockVal events = do
      now <- Time.getCurrentTime
      let oscEvents = formatEvent now clockVal bpm <$> events
      _ <- traverse (NetworkBS.send superDirtSocket . toOSCBundleTest) oscEvents
      return ()

    env :: Env
    env = MkEnv { superDirtSocket, clockRef, signalRef, sendEvents }
  return env

_makeLocalUDPConnection :: Network.PortNumber -> IO Network.Socket
_makeLocalUDPConnection portNumber = do
  (a:_) <- Network.getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNumber))
  superDirtSocket <- Network.socket (Network.addrFamily a) Network.Datagram Network.defaultProtocol
  Network.connect superDirtSocket (Network.addrAddress a)
  return superDirtSocket


delayOneBeat :: Int -> Int -> IO ()
delayOneBeat bpm spb = threadDelay ((10^6 * 60) `div` bpm `div` spb)

main :: IO ()
main = do
  let bpm = 60
  let tpb = 24
  let superDirtPortNumber = 57120
  let config = MkConfig { bpm, tpb, superDirtPortNumber}
  MkEnv{signalRef, sendEvents, clockRef} <- makeEnv config
  modifyMVar_ signalRef (const . return $ embed "bd")
  forever $ do
    sig <- readMVar signalRef
    clockVal <- modifyMVar clockRef (\x -> return (x + (1/fromIntegral tpb), x))
    let events = signal (pruneSignal sig) (clockVal, clockVal + (1/fromIntegral tpb))
    sendEvents clockVal events
    delayOneBeat bpm tpb
