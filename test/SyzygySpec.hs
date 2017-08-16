{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module SyzygySpec where

import Test.Hspec
import Syzygy
import Data.Monoid ((<>))
import Data.Function ((&))
import qualified Data.Time as Time
import qualified Test.QuickCheck as QC
import Control.Concurrent.MVar (newMVar, modifyMVar_, readMVar)

-- FIXME: write better hspec matcher
shouldBeAround :: (Ord a, Num a) => a -> (a, a) -> IO ()
shouldBeAround value (expectedValue, tolerance) =
  abs (value - expectedValue) < tolerance `shouldBe` True

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

    describe "fast" $ do
      let pat = embed ()
      it "should noop for fast 1" $ do
        signal (fast 1 pat) (0, 1)  `shouldBe` signal pat (0, 1)

      it "should work for fast 2" $ do
        signal (fast 2 pat) (0, 0.5) `shouldBe` [MkEvent (0, 0.5) ()]
        signal (fast 2 pat) (0, 1) `shouldBe`   [MkEvent (0, 0.5) (), MkEvent (0.5, 1) ()]
        signal (fast 2 pat) (1, 2) `shouldBe`   [MkEvent (1, 1.5) (), MkEvent (1.5, 2) ()]

      it "should work for fast 3" $ do
        signal (fast 3 pat) (0, (1/3)) `shouldBe` [MkEvent (0, (1/3)) ()]
        signal (fast 3 pat) (0, 1) `shouldBe`     [MkEvent (0, (1/3)) (), MkEvent ((1/3), (2/3)) (), MkEvent ((2/3), 1) ()]
        signal (fast 3 pat) ((2/3), (4/3)) `shouldBe` [MkEvent ((2/3), 1) (), MkEvent (1, (4/3)) ()]

      it "should work for fast 0.5" $ do
        signal (fast 0.5 pat) (0, 1) `shouldBe` [MkEvent (0, 2) ()]
        signal (fast 0.5 pat) (0, 2) `shouldBe` [MkEvent (0, 2) ()]

    describe "shift" $ do
      let pat = embed ()
      it "should noop for shift 0" $ do
        signal (shift 0 pat) (0, 1)  `shouldBe` signal pat (0, 1)

      it "should work" $ do
        signal (shift 0 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]
        signal (shift 0.5 pat) (0, 1) `shouldBe` [MkEvent ((-1/2), (1/2)) (), MkEvent ((1/2), (3/2)) ()]
        signal (shift 1 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]

      it "should shift forwards in time" $ do
        signal (shift 0.25 pat) (0, 1) `shouldBe` [MkEvent ((-3/4), (1/4)) (), MkEvent ((1/4), (5/4)) ()]

    describe "stack" $ do
      let pat = embed ()
      it "should stack patterns" $ do
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

    describe "querySignalNow" $ do
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


  describe "action" $ do
    describe "timing" $ do
      let
        mkTestEnv :: IO Env
        mkTestEnv = do
          beatRef <- newMVar 0
          signalRef <- newMVar mempty
          let action = makeAction (const $ return ()) beatRef signalRef
          let superDirtSocket = undefined
          return MkEnv{superDirtSocket, beatRef, signalRef, action }

      it "has a synchronous delay of (1/60)s when given 60cps" $ do
        MkEnv{action} <- mkTestEnv
        start <- Time.getCurrentTime
        action 60
        end <- Time.getCurrentTime
        (end `Time.diffUTCTime` start) `shouldBeAround` (1/60, 1e-3)

      it "has a synchronous delay of (1/30)s when given 30cps" $ do
        MkEnv{action} <- mkTestEnv
        start <- Time.getCurrentTime
        action 30
        end <- Time.getCurrentTime
        (end `Time.diffUTCTime` start) `shouldBeAround` (1/30, 1e-3)

  describe "when running the action on loop" $ do
    it "has minimal clock drift" $ pending
