{-# LANGUAGE OverloadedStrings #-}

module SyzygySpec where

import Test.Hspec
import Syzygy
import Data.Function ((&))
import Data.Monoid

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
        pat (0.5, 1.5) `shouldBe` [((1, 2), ())]
        pat (0.5, 2.5) `shouldBe` [((1, 2), ()), ((2, 3), ())]

      it "should return nothing for infinitesimally small queries" $ do
        (pat (0, 0)) `shouldBe` []

      it "should have transparently divisible queries" $ do
        (pat (0, 0) <> pat (0, 1)) `shouldBe` pat (0, 1)
        (pat (0, 1) <> pat (1, 1)) `shouldBe` pat (0, 1)
        (pat (0, 0.5) <> pat (0.5, 1.0)) `shouldBe` pat (0, 1)
        (pat (0, 0.3) <> pat (0.3, 1.3) <> pat (1.3, 2)) `shouldBe` pat (0, 2)

    describe "fast" $ do
      let pat = embed ()
      it "should noop for fast 1" $ do
        (fast 1 pat) (0, 1)  `shouldBe`(fast 1 pat) (0, 1)

      it "should work for fast 2" $ do
        (fast 2 pat) (0, 0.5) `shouldBe` [((0, 1/2), ())]
        (fast 2 pat) (0, 1) `shouldBe` [((0, 1/2), ()), ((1/2, 1), ())]
        (fast 2 pat) (1, 2) `shouldBe` [((1, 3/2), ()), ((3/2, 2), ())]

      it "should work for fast 3" $ do
        (fast 3 pat) (0, 1/3) `shouldBe` [((0, 1/3), ())]
        (fast 3 pat) (0, 1) `shouldBe` [((0, 1/3), ()), ((1/3, 2/3), ()), ((2/3, 1), ())]
        (fast 3 pat) (2/3, 4/3) `shouldBe` [((2/3, 1), ()), ((1, 4/3), ())]
