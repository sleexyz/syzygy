{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module SyzygySpec where

import Test.Hspec
import Syzygy
import qualified Test.QuickCheck as QC
import Data.Monoid ((<>))

spec :: Spec
spec = do
  describe "Syzygy" $ do

    describe "Event" $ do
      describe "combineEvent" $ do
        it "works overlapped 1" $ do
          let
            eventX = MkEvent { query = (0, 1), payload = "Hello"}
            eventY = MkEvent { query = (0, 1), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe` [ MkEvent { query = (0, 1), payload = "HelloWorld" } ]

        it "works overlapped 2" $ do
          let
            eventX = MkEvent { query = (0, 0.5), payload = "Hello"}
            eventY = MkEvent { query = (0, 1), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 0.5), payload = "HelloWorld" }
            , MkEvent { query = (0.5, 1), payload = "World" }
            ]

        it "works overlapped 3" $ do
          let
            eventX = MkEvent { query = (0, 1), payload = "Hello"}
            eventY = MkEvent { query = (0, 0.5), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 0.5), payload = "HelloWorld" }
            , MkEvent { query = (0.5, 1), payload = "Hello" }
            ]

        it "works overlapped 4" $ do
          let
            eventX = MkEvent { query = (0, 1), payload = "Hello"}
            eventY = MkEvent { query = (0.5, 1), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 0.5), payload = "Hello" }
            , MkEvent { query = (0.5, 1), payload = "HelloWorld" }
            ]

        it "works overlapped 5" $ do
          let
            eventX = MkEvent { query = (0.5, 1), payload = "Hello"}
            eventY = MkEvent { query = (0, 1), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 0.5), payload = "World" }
            , MkEvent { query = (0.5, 1), payload = "HelloWorld" }
            ]

        it "works overlapped 6" $ do
          let
            eventX = MkEvent { query = (0, 1), payload = "Hello"}
            eventY = MkEvent { query = (0.25, 0.75), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 0.25), payload = "Hello" }
            , MkEvent { query = (0.25, 0.75), payload = "HelloWorld" }
            , MkEvent { query = (0.75, 1), payload = "Hello" }
            ]

        it "works when there are no overlaps" $ do
          let
            eventX = MkEvent { query = (0, 1), payload = "Hello"}
            eventY = MkEvent { query = (1, 2), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 1), payload = "Hello" }
            , MkEvent { query = (1, 2), payload = "World" }
            ]

        it "works when there are no overlaps 2" $ do
          let
            eventX = MkEvent { query = (0, 1), payload = "Hello"}
            eventY = MkEvent { query = (2, 3), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 1), payload = "Hello" }
            , MkEvent { query = (2, 3), payload = "World" }
            ]

        it "works when there are no overlaps 3" $ do
          let
            eventX = MkEvent { query = (2, 3), payload = "Hello"}
            eventY = MkEvent { query = (0, 1), payload = "World"}
          (eventX `combineEvent` eventY) `shouldBe`
            [ MkEvent { query = (0, 1), payload = "World" }
            , MkEvent { query = (2, 3), payload = "Hello" }
            ]

    describe "Signal" $ do
      describe "embed" $ do
        let pat = embed ()
        it "should work" $ do
          signal pat (0, 1) `shouldBe`     [MkEvent (0, 1) ()]
          signal pat (0, 2) `shouldBe`     [MkEvent (0, 1) (), MkEvent (1, 2) ()]
          signal pat (0.5, 1.5) `shouldBe` [MkEvent (0, 1) (), MkEvent (1, 2) ()]
          signal pat (0.5, 2.5) `shouldBe` [MkEvent (0, 1) (), MkEvent (1, 2) (), MkEvent (2, 3) ()]

        it "starts of events should be less than query ends" $ QC.property $ \query@(_, end) ->
          [] == filter (\MkEvent { query = (s, _) } -> s >= end) (signal pat query)

        it "ends of events should be greater than query starts" $ QC.property $ \query@(start, _) ->
          [] == filter (\MkEvent { query = (_, e) } -> e <= start) (signal pat query)

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

      it "should work with monoid" $ do
        signal ((fast 2 $ embed "a") <> (embed "b")) (0, 1) `shouldBe` [MkEvent (0, 0.5) "ab", MkEvent (0.5, 1) "ab"]
        signal ((embed "a") <> (fast 2 $ embed "b")) (0, 1) `shouldBe` [MkEvent (0, 0.5) "ab", MkEvent (0.5, 1) "ab"]
        signal ((fast 2 $ embed "a") <> (fast 2 $ embed "b")) (0, 1) `shouldBe` [MkEvent (0, 0.5) "ab", MkEvent (0.5, 1) "ab"]

    -- describe "shift" $ do
    --   let pat = embed ()
    --   it "should noop for shift 0" $ do
    --     (shift 0 pat) (MkInterval 0 1)  `shouldBe` pat (MkInterval 0 1)

    --   it "should work" $ do
    --     (shift 0 pat)   (MkInterval 0 1) `shouldBe`   [MkSignalEvent (MkInterval 0 1) (pure ())]
    --     (shift 0.5 pat) (MkInterval 0 1) `shouldBe`   [MkSignalEvent (MkInterval (-1/2) (1/2)) (pure ()), MkSignalEvent (MkInterval (1/2) (3/2)) (pure ())]
    --     (shift 1 pat)   (MkInterval 0 1) `shouldBe`   [MkSignalEvent (MkInterval 0 1) (pure ())]

    --   it "should shift forwards in time" $ do
    --     (shift 0.25 pat) (MkInterval 0 1)  `shouldBe` [MkSignalEvent (MkInterval (-3/4) (1/4)) (pure ()), MkSignalEvent (MkInterval (1/4) (5/4)) (pure ())]

    -- describe "stack" $ do
    --   let pat = embed ()
    --   it "should stack patterns" $ do
    --     stack [(shift 0.25 pat), (shift 0.5 pat)] (MkInterval 0 1) `shouldBe`
    --       [ MkSignalEvent (MkInterval (-3/4) (1/4)) (pure ())
    --       , MkSignalEvent (MkInterval (1/4) (5/4)) (pure ())
    --       , MkSignalEvent (MkInterval (-1/2) (1/2)) (pure ())
    --       , MkSignalEvent (MkInterval (1/2) (3/2)) (pure ())
    --       ]

    -- describe "interleave" $ do
    --   let pat = embed ()
    --   it "should noop for 1" $ do
    --     interleave [pat] (MkInterval 0 1) `shouldBe` pat (MkInterval 0 1)

    --   it "should stack patterns, shifted" $ do
    --     interleave [pat, pat]      (MkInterval 0 1) `shouldBe` stack [(shift 0 pat), (shift 0.5 pat)] (MkInterval 0 1)
    --     interleave [pat, pat, pat] (MkInterval 0 1) `shouldBe` stack [(shift 0 pat), (shift (1/3) pat), (shift (2/3) pat)] (MkInterval 0 1)

    -- describe "ap" $ do
    --   let pat = embed ()

    --   it "should noop for pure id" $ do
    --     (ap (embed id) pat) (0, 1)  `shouldBe` [MkSignalEvent (0, 1) ()]
