{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module SyzygySpec where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar, newEmptyMVar, MVar, putMVar, takeMVar)
import Control.Monad (forever)
import Data.Function ((&))
import Data.Monoid ((<>))
import Syzygy
import TestUtils (shouldBeAround, withMockOSCServer, diffTimestamp)
import Test.Hspec
import Vivid.OSC (OSCBundle(..), OSCDatum(OSC_S), decodeOSCBundle, OSC(..), utcToTimestamp, Timestamp(..))

import qualified Data.Time as Time
import qualified Data.ByteString as BS
import qualified Test.QuickCheck as QC
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS

spec :: Spec
spec = do
  describe "Syzygy" $ do
    describe "Signal" $ do
      describe "embed" $ do
        let pat = embed ()
        it "should work" $ do
          signal pat (0, 1) `shouldBe`     [MkEvent (0, 1) ()]
          signal pat (0, 2) `shouldBe`     [MkEvent (0, 1) (), MkEvent (1, 2) ()]
          signal pat (0.5, 1.5) `shouldBe` [MkEvent (0, 1) (), MkEvent (1, 2) ()]
          signal pat (0.5, 2.5) `shouldBe` [MkEvent (0, 1) (), MkEvent (1, 2) (), MkEvent (2, 3) ()]

        it "starts of events should be less than query ends" $ QC.property $ \query@(_, end) ->
          [] == filter (\MkEvent { interval = (s, _) } -> s >= end) (signal pat query)

        it "ends of events should be greater than query starts" $ QC.property $ \query@(start, _) ->
          [] == filter (\MkEvent { interval = (_, e) } -> e <= start) (signal pat query)

        it "should have transparently divisible queries when pruned" $ do
          let pat = pruneSignal $ embed ()
          (signal pat (0, 0)   <> signal pat (0, 1)) `shouldBe` signal pat (0, 1)
          (signal pat (0, 1)   <> signal pat (1, 1)) `shouldBe` signal pat (0, 1)
          (signal pat (0, 0.5) <> signal pat (0.5, 1.0)) `shouldBe` signal pat (0, 1)
          (signal pat (0, 0.3) <> signal pat (0.3, 1.3) <> signal pat (1.3, 2)) `shouldBe` signal pat (0, 2)

      describe "Monoid Instance" $ do
        let
          shouldEqualSignal :: (Show a, Monoid a, Eq a) => Signal a -> Signal a -> IO ()
          shouldEqualSignal x y = signal x query `shouldBe` signal y query
            where
              query = (0, 1)

          checkLeftUnitalLaw x = (mempty <> x) `shouldEqualSignal` x
          checkRightUnitalLaw x = (x <> mempty) `shouldEqualSignal` x
          checkAssociativeLaw x y z = (x <> (y <> z)) `shouldEqualSignal` ((x <> y) <> z)

        it "obeys the left unital law" $ do
          checkLeftUnitalLaw (embed ())
          checkLeftUnitalLaw (fast 2 $ embed ())

        it "obeys the right unital law" $ do
          checkRightUnitalLaw (embed ())
          checkRightUnitalLaw (fast 2 $ embed ())

        it "obeys the associative law" $ do
          checkAssociativeLaw (embed "a") (embed "b") (embed "c")
          checkAssociativeLaw (fast 2 $ embed "a") (fast 3 $ embed "b") (fast 5 $ embed "c")

    describe "pattern combinators" $ do
      describe "fast" $ do
        let pat = embed ()
        it "should noop for fast 1" $ do
          signal (fast 1 pat) (0, 1)  `shouldBe` signal pat (0, 1)

        it "should return appropriate events for fast 2" $ do
          signal (fast 2 pat) (0, 0.5) `shouldBe` [MkEvent (0, 0.5) ()]
          signal (fast 2 pat) (0, 1) `shouldBe`   [MkEvent (0, 0.5) (), MkEvent (0.5, 1) ()]
          signal (fast 2 pat) (1, 2) `shouldBe`   [MkEvent (1, 1.5) (), MkEvent (1.5, 2) ()]

        it "should return appropriate events for fast 3" $ do
          signal (fast 3 pat) (0, (1/3)) `shouldBe` [MkEvent (0, (1/3)) ()]
          signal (fast 3 pat) (0, 1) `shouldBe`     [MkEvent (0, (1/3)) (), MkEvent ((1/3), (2/3)) (), MkEvent ((2/3), 1) ()]
          signal (fast 3 pat) ((2/3), (4/3)) `shouldBe` [MkEvent ((2/3), 1) (), MkEvent (1, (4/3)) ()]

        it "should return appropriate events for fast 0.5" $ do
          signal (fast 0.5 pat) (0, 1) `shouldBe` [MkEvent (0, 2) ()]
          signal (fast 0.5 pat) (0, 2) `shouldBe` [MkEvent (0, 2) ()]

      describe "shift" $ do
        let pat = embed ()
        it "should noop for shift 0" $ do
          signal (shift 0 pat) (0, 1)  `shouldBe` signal pat (0, 1)

        it "should return appropriate events" $ do
          signal (shift 0 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]
          signal (shift 0.5 pat) (0, 1) `shouldBe` [MkEvent ((-1/2), (1/2)) (), MkEvent ((1/2), (3/2)) ()]
          signal (shift 1 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]

        it "should shift forwards in time" $ do
          signal (shift 0.25 pat) (0, 1) `shouldBe` [MkEvent ((-3/4), (1/4)) (), MkEvent ((1/4), (5/4)) ()]

      describe "stack" $ do
        let pat = embed ()
        it "should return appropriate events" $ do
          signal (stack [(shift 0.25 pat), (shift 0.5 pat)]) (0, 1) `shouldBe`
            [ MkEvent ((-3/4), (1/4)) ()
            , MkEvent ((1/4), (5/4)) ()
            , MkEvent ((-1/2), (1/2)) ()
            , MkEvent ((1/2), (3/2)) ()
            ]

      describe "interleave" $ do
        let pat = embed ()
        it "should noop for 1" $ do
          signal (interleave [pat]) (0, 1) `shouldBe` signal pat (0, 1)

        it "should stack patterns, shifted" $ do
          signal (interleave [pat, pat])      (0, 1) `shouldBe` signal (stack [(shift 0 pat), (shift 0.5 pat)]) (0, 1)
          signal (interleave [pat, pat, pat]) (0, 1) `shouldBe` signal (stack [(shift 0 pat), (shift (1/3) pat), (shift (2/3) pat)]) (0, 1)

    describe "querySignal" $ do
      let
        cps :: Rational
        cps = 1

        query :: Interval
        query = (0, 1)

        sig :: Signal String
        sig = fast 3 $ embed "hello"

      it "returns the same payloads from querying the signal, but pruned" $ do
        let
          expectedPayloads :: [String]
          expectedPayloads = (signal (pruneSignal sig) query)& fmap payload
        now <- Time.getCurrentTime
        let oscEvents = querySignal now cps query sig
        (oscEvents & fmap snd) `shouldBe` expectedPayloads

      it "returns the correct future timestamps" $ do
        now <- Time.getCurrentTime
        let
          event1, event2, event3 :: (Time.UTCTime, String)
          [event1, event2, event3] = querySignal now cps query sig

          -- | Difference from now in seconds, with a picosecond tolerance
          shouldExpectDifferenceFromNow :: (Time.UTCTime, a) -> Time.NominalDiffTime -> IO ()
          shouldExpectDifferenceFromNow oscEvent difference = do
              let
                (futureTimestamp, _) = oscEvent
              (futureTimestamp `Time.diffUTCTime` now) `shouldBeAround` (difference, 1e-9)

        event1 `shouldExpectDifferenceFromNow` (1 * 0/3)
        event2 `shouldExpectDifferenceFromNow` (1 * 1/3)
        event3 `shouldExpectDifferenceFromNow` (1 * 2/3)

    describe "sendEvents" $ do
      let
        mkTestEnvWithNoHandler :: IO Env
        mkTestEnvWithNoHandler = mkTestEnv (\_ -> return ())

        mkTestEnv :: (OSCBundle -> IO ()) -> IO Env
        mkTestEnv handler = withMockOSCServer handler $ makeEnv

      describe "timing" $ do
        it "has a synchronous delay of (1/60)s when given rate of 60cps" $ do
          MkEnv{sendEvents} <- mkTestEnvWithNoHandler
          start <- Time.getCurrentTime
          sendEvents 60
          end <- Time.getCurrentTime
          (end `Time.diffUTCTime` start) `shouldBeAround` (1/60, 2e-3)

        it "has a synchronous delay of (1/30)s when given rate of 30cps" $ do
          MkEnv{sendEvents} <- mkTestEnvWithNoHandler
          start <- Time.getCurrentTime
          sendEvents 30
          end <- Time.getCurrentTime
          (end `Time.diffUTCTime` start) `shouldBeAround` (1/30, 2e-3)

      it "increments the clockRef by one cycle" $ do
        MkEnv{sendEvents, clockRef} <- mkTestEnvWithNoHandler
        before <- readMVar clockRef
        sendEvents 60
        after <- readMVar clockRef
        (after - before) `shouldBe` 1

      describe "when talking to SuperDirt" $ do
        let
          cps :: Num a => a
          cps = 60

          -- | sends one cycle to the mock SuperDirt at 60cps
          sendOneCycle :: Signal BS.ByteString -> IO (IO (), Chan OSCBundle)
          sendOneCycle signal = do
            (oscBundleChan :: Chan OSCBundle) <- newChan
            MkEnv{sendEvents, clockRef, signalRef} <- mkTestEnv $ \bundle -> do
                writeChan oscBundleChan bundle
            modifyMVar_ signalRef (const . return $ signal)
            return (sendEvents cps, oscBundleChan)

        it "can send an event to SuperDirt" $ do
          (sendEvents, oscBundleChan) <- sendOneCycle (embed "bd")
          now <- Time.getCurrentTime
          do
            sendEvents
            do
              OSCBundle timestamp [Right message] <- readChan oscBundleChan
              message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
              (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (0, 1e-3)

        it "can send multiple event to SuperDirt in the same cycle" $ do
          (sendEvents, oscBundleChan) <- sendOneCycle (interleave [ embed "bd", embed "sn" ])
          now <- Time.getCurrentTime
          do
            sendEvents
            do
              OSCBundle timestamp [Right message] <- readChan oscBundleChan
              message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
              (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (1/cps * 0/2, 1e-3)
            do
              OSCBundle timestamp [Right message] <- readChan oscBundleChan
              message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])
              (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (1/cps * 1/2, 1e-3)

        it "can send multiple events in multiple cycles, when invoked multiple times" $ do
          (sendEvents, oscBundleChan) <- sendOneCycle (interleave [ fast 0.5 $ embed "bd", embed "sn" ])
          now <- Time.getCurrentTime
          do
            sendEvents
            do
              OSCBundle timestamp [Right message] <- readChan oscBundleChan
              message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "bd"])
              (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (1/cps * 0/2, 1e-3)
            do
              OSCBundle timestamp [Right message] <- readChan oscBundleChan
              message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])
              (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (1/cps * 1/2, 1e-3)
          do
            sendEvents
            do
              OSCBundle timestamp [Right message] <- readChan oscBundleChan
              message `shouldBe` (OSC "/play2" [OSC_S "s", OSC_S "sn"])
              (timestamp `diffTimestamp` utcToTimestamp  now) `shouldBeAround` (1/cps * 3/2, 1e-3)

    describe "when running sendEvents on loop" $ do
      it "has minimal clock drift" $ pending
