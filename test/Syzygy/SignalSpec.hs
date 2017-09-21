
module Syzygy.SignalSpec where

import Data.Function ((&))
import Data.Monoid ((<>))
import Test.Hspec
import qualified Test.QuickCheck as QC

import Syzygy.Signal

spec :: Spec
spec = do
  describe "splitIntervals" $ do
    it "works" $ do
      splitIntervals (0, 1) `shouldBe` [(0, 1)]
      splitIntervals (0, 2) `shouldBe` [(0, 1), (1, 1)]
      splitIntervals (0.5, 0.5) `shouldBe` [(0.5, 0.5)]
      splitIntervals (0.5, 1) `shouldBe` [(0.5, 0.5), (1, 0.5)]
      splitIntervals (0.5, 2) `shouldBe` [(0.5, 0.5), (1, 1), (2, 0.5)]

  describe "Signal" $ do
    describe "embed" $ do
      it "works" $ do
        let pat = embed ()
        signal pat (0, 1) `shouldBe`     [MkEvent (0, 1) ()]
        signal pat (0, 2) `shouldBe`     [MkEvent (0, 1) (), MkEvent (1, 1) ()]
        signal pat (0.5, 1.5) `shouldBe` [MkEvent (1, 1) ()]
        signal pat (0.5, 2.5) `shouldBe` [MkEvent (1, 1) (), MkEvent (2, 1) ()]

      it "starts of events should be less than query ends" $ QC.property $ \query@(start, dur) ->
        let pat = embed () in
        [] == filter (\MkEvent { interval = (s, _) } -> s >= start + dur) (signal pat query)

      it "ends of events should be greater than query starts" $ QC.property $ \query@(start, _) ->
        let pat = embed () in
        [] == filter (\MkEvent { interval = (s, d) } -> (s + d) <= start) (signal pat query)

      it "has transparently divisible queries when pruned" $ do
        let pat = pruneSignal $ embed ()
        (signal pat (0, 0)   <> signal pat (0, 1)) `shouldBe` signal pat (0, 1)
        (signal pat (0, 1)   <> signal pat (1, 0)) `shouldBe` signal pat (0, 1)
        (signal pat (0, 0.5) <> signal pat (0.5, 0.5)) `shouldBe` signal pat (0, 1)
        (signal pat (0, 0.3) <> signal pat (0.3, 1) <> signal pat (1.3, 0.7)) `shouldBe` signal pat (0, 2)

    describe "Monoid Instance" $ do
      let
        shouldEqualSignal :: (Show a, Monoid a, Eq a) => Signal a -> Signal a -> IO ()
        shouldEqualSignal x y = signal x query `shouldBe` signal y query
          where
            query = (0, 1)

        checkLeftUnitalLaw :: Signal () -> IO ()
        checkLeftUnitalLaw x = (mempty <> x) `shouldEqualSignal` x

        checkRightUnitalLaw :: Signal () -> IO ()
        checkRightUnitalLaw x = (x <> mempty) `shouldEqualSignal` x

        checkAssociativeLaw :: Signal String -> Signal String -> Signal String -> IO ()
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

      it "allows us to stack signals" $ do
        let pat = embed ()
        signal (mconcat [(shift 0.25 pat), (shift 0.5 pat)]) (0, 1) `shouldBe`
          [ MkEvent ((1/4), 1)  ()
          , MkEvent ((1/2), 1) ()
          ]

  describe "Pattern Combinators" $ do
    describe "fast" $ do
      let pat = embed ()
      it "noops for fast 1" $ do
        signal (fast 1 pat) (0, 1)  `shouldBe` signal pat (0, 1)

      it "returns appropriate events for fast 2" $ do
        signal (fast 2 pat) (0, 0.5) `shouldBe` [MkEvent (0, 0.5) ()]
        signal (fast 2 pat) (0, 1) `shouldBe`   [MkEvent (0, 0.5) (), MkEvent (0.5, 0.5) ()]
        signal (fast 2 pat) (1, 1) `shouldBe`   [MkEvent (1, 0.5) (), MkEvent (1.5, 0.5) ()]

      it "returns appropriate events for fast 3" $ do
        signal (fast 3 pat) (0, (1/3)) `shouldBe` [MkEvent (0, 1/3) ()]
        signal (fast 3 pat) (0, 1) `shouldBe`     [MkEvent (0, 1/3) (), MkEvent (1/3, 1/3) (), MkEvent (2/3, 1/3) ()]
        signal (fast 3 pat) ((2/3), (2/3)) `shouldBe` [MkEvent (2/3, 1/3) (), MkEvent (1, 1/3) ()]

      it "returns appropriate events for fast 0.5" $ do
        signal (fast 0.5 pat) (0, 1) `shouldBe` [MkEvent (0, 2) ()]
        signal (fast 0.5 pat) (0, 2) `shouldBe` [MkEvent (0, 2) ()]

    describe "shift" $ do
      let pat = embed ()
      it "noops for shift 0" $ do
        signal (shift 0 pat) (0, 1)  `shouldBe` signal pat (0, 1)

      it "returns appropriate events" $ do
        signal (shift 0 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]
        signal (shift 0.5 pat) (0, 1) `shouldBe` [MkEvent (1/2, 1) ()]
        signal (shift 1 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]

      it "shifts forwards in time" $ do
        signal (shift 0.25 pat) (0, 1) `shouldBe` [MkEvent (1/4, 1) ()]

    describe "nest" $ do
      let
        a, b, c :: Signal String
        a = fast 2 $ embed "a"
        b = fast 2 $ embed "b"
        c = fast 2 $ embed "c"

      it "should noop for 1" $ do
        signal (nest [a]) (0, 1) `shouldBe` signal a (0, 1)

      describe "for one cycle" $ do
        it "should play scaled patterns, shifted in order" $ do
          signal (nest [a, b]) (0, 1) `shouldBe` mempty
            <> signal (shift (0/2) $ fast 2 a) (0, 0.5)
            <> signal (shift (1/2) $ fast 2 b) (0.5, 0.5)

          signal (nest [a, b, c]) (0, 1) `shouldBe` mempty
            <> signal (shift (1/3) $ fast 3 a) (0, 1/3)
            <> signal (shift (1/3) $ fast 3 b) (1/3, 1/3)
            <> signal (shift (2/3) $ fast 3 c) (2/3, 1/3)

      describe "for multiple cycles" $ do
        it "should play scaled patterns, shifted in order" $ do
          signal (nest [a, b]) (0, 2) `shouldMatchList` mempty
            <> signal (shift (0/2) $ fast 2 a) ((0/2),(1/2))
            <> signal (shift (1/2) $ fast 2 b) ((1/2),(1/2))
            <> signal (shift (0/2) $ fast 2 a) ((2/2),(1/2))
            <> signal (shift (1/2) $ fast 2 b) ((3/2),(1/2))

          signal (nest [a, b, c]) (0, 2) `shouldMatchList` mempty
            <> signal (shift (0/3) $ fast 3 a) (0/3, 1/3)
            <> signal (shift (1/3) $ fast 3 b) (1/3, 1/3)
            <> signal (shift (2/3) $ fast 3 c) (2/3, 1/3)
            <> signal (shift (0/3) $ fast 3 a) (3/3, 1/3)
            <> signal (shift (1/3) $ fast 3 b) (4/3, 1/3)
            <> signal (shift (2/3) $ fast 3 c) (5/3, 1/3)

    describe "cat" $ do
      it "should be silent with no elements" $ do
        signal (cat ([] :: [Signal ()])) (0, 1) `shouldMatchList` signal mempty (0, 1)
        signal (cat ([] :: [Signal ()])) (0, 10) `shouldMatchList` signal mempty (0, 10)

      it "should noop given one element" $ do
        let sig = cat [embed "a"]
        signal sig (0, 2) `shouldMatchList`
          [ MkEvent { interval=(0, 1), payload="a" }
          , MkEvent { interval=(1, 1), payload="a"}
          ]

      it "should alternate between signals given two elements" $ do
        let sig = cat [embed "a", embed "b"]
        signal sig (0, 4) `shouldMatchList`
          [ MkEvent { interval=(0, 1), payload="a" }
          , MkEvent { interval=(1, 1), payload="b"}
          , MkEvent { interval=(2, 1), payload="a" }
          , MkEvent { interval=(3, 1), payload="b"}
          ]

      it "should alternate between signals and not drop events" $ do
        let sigA = cat [embed "a0", embed "a1"]
        let sigB = cat [embed "b0", embed "b1"]
        let sig = cat [sigA, sigB]
        signal sig (0, 1) `shouldMatchList`
          [ MkEvent {interval=(0, 1), payload="a0"}
          ]
        signal sig (0, 2) `shouldMatchList`
          [ MkEvent {interval=(0, 1), payload="a0"}
          , MkEvent {interval=(1, 1), payload="b0"}
          ]
        signal sig (0, 4) `shouldMatchList`
          [ MkEvent {interval=(0, 1), payload="a0"}
          , MkEvent {interval=(1, 1), payload="b0"}
          , MkEvent {interval=(2, 1), payload="a1"}
          , MkEvent {interval=(3, 1), payload="b1"}
          ]

      it "should alternate between signals and not drop events" $ do
        let sigA = cat [embed "a0", embed "a1"] & fast 2
        let sigB = cat [embed "b0", embed "b1"] & fast 2
        let sigC = cat [embed "c0", embed "c1"] & fast 2
        let sig = cat [sigA, sigB, sigC]

        sequence $ (print$) <$> signal sig (0, 6)
        return ()
