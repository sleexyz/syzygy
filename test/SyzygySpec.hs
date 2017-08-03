{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module SyzygySpec where

import Test.Hspec
import Syzygy
import Data.Monoid
import Test.QuickCheck
import Data.Function ((&))

queryWith :: Interval -> Signal a -> [Event a]
queryWith x p = p x

spec :: Spec
spec = do
  describe "Syzygy" $ do
    describe "embed" $ do
      let pat = embed ()
      it "should work" $ do
        pat (0, 1) `shouldBe` [((0, 1), ())]
        pat (0, 2) `shouldBe` [((0, 1), ()), ((1, 2), ())]
        pat (0.5, 1.5) `shouldBe` [((0, 1), ()), ((1, 2), ())]
        pat (0.5, 2.5) `shouldBe` [((0, 1), ()), ((1, 2), ()), ((2, 3), ())]

      it "starts of events should be less than query ends" $ property $ \input ->
        let
          ((NonNegative (start :: Rational), NonNegative (dur :: Rational))) = input
          end = start + dur
          events = pat (start, end)
          offendingEvents = filter (\((s, e), _) -> s >= end) events
        in
          length offendingEvents == 0

      it "ends of events should be greater than query starts" $ property $ \input ->
        let
          ((NonNegative (start :: Rational), NonNegative (dur :: Rational))) = input
          end = start + dur
          events = pat (start, end)
          offendingEvents = filter (\((s, e), _) -> e <= start) events
        in
          length offendingEvents == 0

      describe "when pruned" $ do
        let pat = embed () & prune

        it "should have transparently divisible queries" $ do
          (pat (0, 0) <> pat (0, 1)) `shouldBe` pat (0, 1)
          (pat (0, 1) <> pat (1, 1)) `shouldBe` pat (0, 1)
          (pat (0, 0.5) <> pat (0.5, 1.0)) `shouldBe` pat (0, 1)
          (pat (0, 0.3) <> pat (0.3, 1.3) <> pat (1.3, 2)) `shouldBe` pat (0, 2)

    describe "fast" $ do
      let pat = embed ()
      it "should noop for fast 1" $ do
        (fast 1 pat) (0, 1)  `shouldBe` pat (0, 1)

      it "should work for fast 2" $ do
        (fast 2 pat) (0, 0.5) `shouldBe` [((0, 1/2), ())]
        (fast 2 pat) (0, 1) `shouldBe` [((0, 1/2), ()), ((1/2, 1), ())]
        (fast 2 pat) (1, 2) `shouldBe` [((1, 3/2), ()), ((3/2, 2), ())]

      it "should work for fast 3" $ do
        (fast 3 pat) (0, 1/3) `shouldBe` [((0, 1/3), ())]
        (fast 3 pat) (0, 1) `shouldBe` [((0, 1/3), ()), ((1/3, 2/3), ()), ((2/3, 1), ())]
        (fast 3 pat) (2/3, 4/3) `shouldBe` [((2/3, 1), ()), ((1, 4/3), ())]

      it "should noop for fast 0.5" $ do
        (fast 0.5 pat) (0, 1)  `shouldBe` [((0, 2), ())]
        (fast 0.5 pat) (0, 2)  `shouldBe` [((0, 2), ())]

    describe "shift" $ do
      let pat = embed ()
      it "should noop for shift 0" $ do
        (shift 0 pat) (0, 1)  `shouldBe` pat (0, 1)

      it "should work" $ do
        (shift 0 pat) (0, 1)  `shouldBe` [((0, 1), ())]
        (shift 0.5 pat) (0, 1)  `shouldBe` [((-1/2, 1/2), ()), ((1/2, 3/2), ())]
        (shift 1 pat) (0, 1)  `shouldBe` [((0, 1), ())]

      it "should shift forwards in time" $ do
        (shift 0.25 pat) (0, 1)  `shouldBe` [((-3/4, 1/4), ()), ((1/4, 5/4), ())]

    describe "stack" $ do
      let pat = embed ()
      it "should stack patterns" $ do
        stack [(shift 0.25 pat), (shift 0.5 pat)] (0, 1) `shouldBe` [((-3/4, 1/4), ()), ((1/4, 5/4), ()), ((-1/2, 1/2), ()), ((1/2, 3/2), ())]

    describe "interleave" $ do
      let pat = embed ()
      it "should noop for 1" $ do
        interleave [pat] (0, 1) `shouldBe` pat (0, 1)

      it "should stack patterns, shifted" $ do
        interleave [pat, pat] (0, 1) `shouldBe` stack [(shift 0 pat), (shift 0.5 pat)] (0, 1)
        interleave [pat, pat, pat] (0, 1) `shouldBe` stack [(shift 0 pat), (shift (1/3) pat), (shift (2/3) pat)] (0, 1)

    describe "ap" $ do
      let pat = embed ()

      it "should noop for pure id" $ do
        (ap (embed id) pat) (0, 1)  `shouldBe` [((0, 1), ())]

      it "should work" $ do
        let patF = interleave $ embed <$> [("a",), ("b",)]
        (ap patF pat) (0, 1) `shouldBe` [((0, 1), ("a", ())), ((0, 1), ("b", ()))] -- FIXME: is this really what we want?
        (ap patF (fast 2 pat)) (0, 1)  `shouldBe` [((0, 1/2), ("a", ())), ((1/2, 1), ("a", ())), ((0, 1/2), ("b", ())), ((1/2, 1), ("b", ()))]
