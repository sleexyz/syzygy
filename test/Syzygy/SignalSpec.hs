
module Syzygy.SignalSpec where

import Data.Function ((&))
import Data.Monoid ((<>))
import Test.Hspec
import qualified Test.QuickCheck as QC

import Syzygy.Signal

spec :: Spec
spec = do
  describe "Signal" $ do
    describe "embed" $ do
      it "works" $ do
        let pat = embed ()
        signal pat (0, 1) `shouldBe`     [MkEvent (0, 1) ()]
        signal pat (0, 2) `shouldBe`     [MkEvent (0, 1) (), MkEvent (1, 1) ()]
        signal pat (0.5, 1.5) `shouldBe` [MkEvent (0, 1) (), MkEvent (1, 1) ()]
        signal pat (0.5, 2.5) `shouldBe` [MkEvent (0, 1) (), MkEvent (1, 1) (), MkEvent (2, 1) ()]

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
          [ MkEvent ((-3/4), 1) ()
          , MkEvent ((1/4), 1)  ()
          , MkEvent ((-1/2), 1) ()
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
        signal (shift 0.5 pat) (0, 1) `shouldBe` [MkEvent (-1/2, 1) (), MkEvent (1/2, 1) ()]
        signal (shift 1 pat)   (0, 1) `shouldBe` [MkEvent (0, 1) ()]

      it "shifts forwards in time" $ do
        signal (shift 0.25 pat) (0, 1) `shouldBe` [MkEvent (-3/4, 1) (), MkEvent (1/4, 1) ()]

  --   describe "_filterSignal" $ do
  --     let
  --       pat :: Signal String
  --       pat = fast 4 $ embed "bd"
  --     it "should return no events when predicate is always false" $ do
  --       let predicate = const False
  --       signal (_filterSignal predicate pat) (0, 2) `shouldBe` mempty

  --     it "should return all events when predicate is always true" $ do
  --       let predicate = const True
  --       signal (_filterSignal predicate pat) (0, 2) `shouldBe` signal pat (0, 2)

  --     it "should be able to use a custom predicate" $ do
  --       let predicate MkEvent{interval= (start, _)} =
  --             let
  --               startFract = (snd $ properFraction @ Rational @ Integer start)
  --             in
  --               startFract >= 0 && startFract < 0.5
  --       signal (_filterSignal predicate pat) (0, 2) `shouldBe` signal pat (0, 0.5) <> signal pat (1, 1.5)

  --   describe "interleave" $ do
  --     let
  --       a, b, c :: Signal String
  --       a = fast 2 $ embed "a"
  --       b = fast 2 $ embed "b"
  --       c = fast 2 $ embed "c"
  --     it "should noop for 1" $ do
  --       signal (interleave [a]) (0, 1) `shouldBe` signal a (0, 1)

  --     describe "for one cycle" $ do
  --       it "should play patterns, shifted in order" $ do
  --         signal (interleave [a, b]) (0, 1) `shouldBe` mempty
  --           <> signal (shift (0/2) a) (0, 0.5)
  --           <> signal (shift (1/2) b) (0.5, 1)

  --         signal (interleave [a, b, c]) (0, 1) `shouldBe` mempty
  --           <> signal (shift (0/3) a) (0, 1/3)
  --           <> signal (shift (1/3) b) (1/3, 2/3)
  --           <> signal (shift (2/3) c) (2/3, 1)

  --     describe "for multiple cycles" $ do
  --       it "should play patterns, shifted in order" $ do
  --         signal (interleave [a, b]) (0, 2) `shouldMatchList` mempty
  --           <> signal (shift (0/2) a) ((0/2),(1/2))
  --           <> signal (shift (1/2) b) ((1/2),(2/2))
  --           <> signal (shift (0/2) a) ((2/2),(3/2))
  --           <> signal (shift (1/2) b) ((3/2),(4/2))

  --         signal (interleave [a, b, c]) (0, 2) `shouldMatchList` mempty
  --           <> signal (shift (0/3) a) (0/3, 1/3)
  --           <> signal (shift (1/3) b) (1/3, 2/3)
  --           <> signal (shift (2/3) c) (2/3, 3/3)
  --           <> signal (shift (0/3) a) (3/3, 4/3)
  --           <> signal (shift (1/3) b) (4/3, 5/3)
  --           <> signal (shift (2/3) c) (5/3, 6/3)

  --   describe "nest" $ do
  --     let
  --       a, b, c :: Signal String
  --       a = fast 2 $ embed "a"
  --       b = fast 2 $ embed "b"
  --       c = fast 2 $ embed "c"
  --     it "should noop for 1" $ do
  --       signal (nest [a]) (0, 1) `shouldBe` signal a (0, 1)

  --     describe "for one cycle" $ do
  --       it "should play scaled patterns, shifted in order" $ do
  --         signal (nest [a, b]) (0, 1) `shouldBe` mempty
  --           <> signal (shift (0/2) $ fast 2 a) (0, 0.5)
  --           <> signal (shift (1/2) $ fast 2 b) (0.5, 1)

  --         signal (nest [a, b, c]) (0, 1) `shouldBe` mempty
  --           <> signal (shift (1/3) $ fast 3 a) (0, 1/3)
  --           <> signal (shift (1/3) $ fast 3 b) (1/3, 2/3)
  --           <> signal (shift (2/3) $ fast 3 c) (2/3, 1)

  --     describe "for multiple cycles" $ do
  --       it "should play scaled patterns, shifted in order" $ do
  --         signal (nest [a, b]) (0, 2) `shouldMatchList` mempty
  --           <> signal (shift (0/2) $ fast 2 a) ((0/2),(1/2))
  --           <> signal (shift (1/2) $ fast 2 b) ((1/2),(2/2))
  --           <> signal (shift (0/2) $ fast 2 a) ((2/2),(3/2))
  --           <> signal (shift (1/2) $ fast 2 b) ((3/2),(4/2))

  --         signal (nest [a, b, c]) (0, 2) `shouldMatchList` mempty
  --           <> signal (shift (0/3) $ fast 3 a) (0/3, 1/3)
  --           <> signal (shift (1/3) $ fast 3 b) (1/3, 2/3)
  --           <> signal (shift (2/3) $ fast 3 c) (2/3, 3/3)
  --           <> signal (shift (0/3) $ fast 3 a) (3/3, 4/3)
  --           <> signal (shift (1/3) $ fast 3 b) (4/3, 5/3)
  --           <> signal (shift (2/3) $ fast 3 c) (5/3, 6/3)

  --   -- describe "cat" $ do
  --     -- let sig = nest [embed "a", embed "b"] & slow 2

  --     -- it "should be silent with 0" $ do
  --     --   signal (cat ([] :: [Signal ()])) (0, 1) `shouldMatchList` signal mempty (0, 1)

  --     -- it "should no-op with 1" $ do
  --     --   signal (cat [sig]) (0, 1) `shouldMatchList` signal sig (0, 1)

  --     -- it "should layer with 2" $ do
  --     --   let sig = cat [slow 2 $ nest [embed "a0", embed "a1"], slow 2 $ nest [embed "b0", embed "b1"]]
  --       -- signal sig (0, 1) `shouldMatchList` [ MkEvent {interval=(0, 1), payload="a0"}]
  --       -- signal sig (0, 2) `shouldMatchList`
  --       --   [ MkEvent {interval=(0, 1), payload="a0"}
  --       --   , MkEvent {interval=(1, 2), payload="b0"}
  --       --   ]
  --       -- signal sig (0, 4) `shouldMatchList`
  --       --   [ MkEvent {interval=(0, 1), payload="a0"}
  --       --   , MkEvent {interval=(1, 2), payload="b0"}
  --       --   , MkEvent {interval=(2, 3), payload="a1"}
  --       --   , MkEvent {interval=(3, 4), payload="b1"}
  --       --   ]

  --     -- it "should layer with 4" $ do
  --     --   sequence $ (print$) <$> signal (cat [slow 2 $ nest [embed "a0", embed "a1"], slow 2 $ nest [embed "b0", embed "b1"]]) (0,4)
  --     --   print "foo"

  --       -- signal (cat [sig, sig]) (0,2) `shouldMatchList`
  --       --   [ MkEvent {interval = (0,1), payload = "a"}
  --       --   , MkEvent {interval = (1,2), payload = "a"}
  --       --   ]

  --     -- it "should only have" $ do
  --     --   print "foo"
  --     --   sequence_ $ (print$) <$> signal (rep 1 sig) (0, 2)
  --     --   print "foo"
  --     --   sequence_ $ (print$) <$> signal (rep 2 sig) (0, 2)
