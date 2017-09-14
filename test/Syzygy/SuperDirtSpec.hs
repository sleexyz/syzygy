
module Syzygy.SuperDirtSpec where

-- import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
-- import Control.Concurrent.MVar (modifyMVar_, readMVar)
-- import Data.Function ((&))
-- import TestUtils
--   ( shouldBeAround
  -- , withMockOSCServer
  -- , diffTimestamp
  -- )
import Test.Hspec
-- import Vivid.OSC (OSCBundle(..), OSCDatum(OSC_S), OSC(..), utcToTimestamp )

-- import qualified Data.Time as Time
-- import qualified Data.ByteString as BS

spec :: Spec
spec = do
  return ()
  -- describe "querySignal" $ do
  --   let
  --     bpm :: Int
  --     bpm = 60

  --     query :: Interval
  --     query = (0, 1)

  --     sig :: Signal String
  --     sig = fast 3 $ embed "hello"

  --   it "returns the same payloads from querying the signal, but pruned" $ do
  --     let
  --       expectedPayloads :: [String]
  --       expectedPayloads = (signal (pruneSignal sig) query) & fmap payload
  --     now <- Time.getCurrentTime
  --     let oscEvents = querySignal now bpm query sig
  --     (oscEvents & fmap snd) `shouldBe` expectedPayloads

  --   it "returns the correct future timestamps" $ do
  --     now <- Time.getCurrentTime
  --     let
  --       event1, event2, event3 :: (Time.UTCTime, String)
  --       [event1, event2, event3] = querySignal now bpm query sig

  --       -- | Difference from now in seconds, with a picosecond tolerance
  --       shouldExpectDifferenceFromNow :: (Time.UTCTime, a) -> Time.NominalDiffTime -> IO ()
  --       shouldExpectDifferenceFromNow oscEvent difference = do
  --           let
  --             (futureTimestamp, _) = oscEvent
  --           (futureTimestamp `Time.diffUTCTime` now) `shouldBeAround` (difference, 1e-9)

  --     event1 `shouldExpectDifferenceFromNow` (1 * 0/3)
  --     event2 `shouldExpectDifferenceFromNow` (1 * 1/3)
  --     event3 `shouldExpectDifferenceFromNow` (1 * 2/3)

  -- describe "sendEvents" $ do
  --   let
  --     mkTestEnvWithNoHandler :: Int -> IO Env
  --     mkTestEnvWithNoHandler bpm = mkTestEnv bpm (\_ -> return ())

  --     mkTestEnv :: Int -> (OSCBundle -> IO ()) -> IO Env
  --     mkTestEnv bpm handler = withMockOSCServer handler $ \portNumber -> makeEnv MkConfig{portNumber, bpm}


  --   describe "when talking to SuperDirt" $ do
  --     let
  --       bpm :: Num a => a
  --       bpm = 60

  --       -- | sends one cycle to the mock SuperDirt at 60cps
  --       sendOneCycle :: Signal BS.ByteString -> IO (IO (), Chan OSCBundle)
  --       sendOneCycle signal = do
  --         (oscBundleChan :: Chan OSCBundle) <- newChan
  --         MkEnv{sendEvents, signalRef} <- mkTestEnv bpm $ \bundle -> do
  --             writeChan oscBundleChan bundle
  --         modifyMVar_ signalRef (const . return $ signal)
  --         return (sendEvents, oscBundleChan)

  --     it "can send an event to SuperDirt" $ do
  --       (sendEvents, oscBundleChan) <- sendOneCycle (embed "bd")
  --       now <- Time.getCurrentTime
  --       do
  --         sendEvents
  --         do
  --           OSCBundle timestamp [Right message] <- readChan oscBundleChan
  --           message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
  --           (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (0, 1e-3)

  --     it "can send multiple event to SuperDirt in the same cycle" $ do
  --       (sendEvents, oscBundleChan) <- sendOneCycle (nest [ embed "bd", embed "sn" ])
  --       now <- Time.getCurrentTime
  --       do
  --         sendEvents
  --         delayOneBeat 60
  --         do
  --           OSCBundle timestamp [Right message] <- readChan oscBundleChan
  --           message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
  --           (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (60/bpm * 0/2, 1e-3)
  --         do
  --           OSCBundle timestamp [Right message] <- readChan oscBundleChan
  --           message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])
  --           (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (60/bpm * 1/2, 1e-3)

  --     it "can send multiple events in multiple cycles, when invoked multiple times" $ do
  --       (sendEvents, oscBundleChan) <- sendOneCycle (interleave [ fast 0.5 $ embed "bd", embed "sn" ])
  --       now <- Time.getCurrentTime
  --       do
  --         sendEvents
  --         delayOneBeat 60
  --         do
  --           OSCBundle timestamp [Right message] <- readChan oscBundleChan
  --           message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
  --           (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (60/bpm * 0/2, 1e-3)
  --         do
  --           OSCBundle timestamp [Right message] <- readChan oscBundleChan
  --           message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])
  --           (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (60/bpm * 1/2, 1e-3)
  --       do
  --         sendEvents
  --         delayOneBeat 60
  --         do
  --           OSCBundle timestamp [Right message] <- readChan oscBundleChan
  --           message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])
  --           (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (60/bpm * 3/2, 1e-2)

