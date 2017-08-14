{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module SyzygySpec where

import Test.Hspec
import Syzygy
import Test.QuickCheck
import Data.Function ((&))
import Data.Monoid ((<>))

spec :: Spec
spec = do
  describe "Syzygy" $ do

    describe "Event" $ do
      -- describe "Applicative Instance" $ do
      --   it "obeys the identity law" $ property $ \(event :: Event ()) ->
      --     (pure id <*> event) ==  event

      --   it "obeys the homomorphism law" $ do
      --     let f = (, "hello")
      --     let x = ()
      --     (pure f <*> pure x) `shouldBe` pure @Event (f x)

      --   it "obeys the interchange law" $ property $ \(query :: Interval)->
      --     let u = MkEvent { query, payload = (,"hello") }
      --     in (u <*> pure ()) == (pure ($()) <*> u)

      --   it "obeys the composition law" $ property $ \(input :: (Event String, Event String, Event String)) ->
      --     let (fmap (<>) -> u, fmap (<>) -> v, x) = input
      --     in (pure (.) <*> u <*> v <*> x) == (u <*> (v <*> x))

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
          signal pat (0.5, 1.5) `shouldBe` [MkEvent (1, 2) ()]
          signal pat (0.5, 2.5) `shouldBe` [MkEvent (1, 2) (), MkEvent (2, 3) ()]

        it "starts of events should be less than query ends" $ property $ \query@(_, end) ->
          [] == filter (\MkEvent { query = (s, _) } -> s >= end) (signal pat query)

        it "ends of events should be greater than query starts" $ property $ \query@(start, _) ->
          [] == filter (\MkEvent { query = (_, e) } -> e <= start) (signal pat query)

        it "should have transparently divisible queries" $ do
          (signal pat (0, 0)   <> signal pat (0, 1)) `shouldBe` signal pat (0, 1)
          (signal pat (0, 1)   <> signal pat (1, 1)) `shouldBe` signal pat (0, 1)
          (signal pat (0, 0.5) <> signal pat (0.5, 1.0)) `shouldBe` signal pat (0, 1)
          (signal pat (0, 0.3) <> signal pat (0.3, 1.3) <> signal pat (1.3, 2)) `shouldBe` signal pat (0, 2)

      -- describe "Monoid Instance" $ do
        -- it "obeys the associative law" $ property $ \(x :: Interval, y :: Interval, z :: Interval) ->
        --   (x <> (y <> z)) ==  ((x <> y) <> z)

        -- it "obeys the left unital law" $ property $ \(x :: Interval) ->
        --   (mempty <> x) == x

        -- it "obeys the right unital law" $ property $ \(x :: Interval) ->
        --   (x <> mempty) == x


    -- describe "fast" $ do
    --   let pat = embed ()
    --   it "should noop for fast 1" $ do
    --     (fast 1 pat) (MkInterval 0 1)  `shouldBe` pat (MkInterval 0 1)

    --   it "should work for fast 2" $ do
    --     (fast 2 pat) (MkInterval 0 0.5) `shouldBe` [MkSignalEvent (MkInterval 0 0.5) (pure ())]
    --     (fast 2 pat) (MkInterval 0 1) `shouldBe`   [MkSignalEvent (MkInterval 0 0.5) (pure ()), MkSignalEvent (MkInterval 0.5 1) (pure ())]
    --     (fast 2 pat) (MkInterval 1 2) `shouldBe`   [MkSignalEvent (MkInterval 1 1.5) (pure ()), MkSignalEvent (MkInterval 1.5 2) (pure ())]

    --   it "should work for fast 3" $ do
    --     (fast 3 pat) (MkInterval 0 (1/3)) `shouldBe`     [MkSignalEvent (MkInterval 0 (1/3)) (pure ())]
    --     (fast 3 pat) (MkInterval 0 1) `shouldBe`         [MkSignalEvent (MkInterval 0 (1/3)) (pure ()), MkSignalEvent (MkInterval (1/3) (2/3)) (pure ()), MkSignalEvent (MkInterval (2/3) 1) (pure ())]
    --     (fast 3 pat) (MkInterval (2/3) (4/3)) `shouldBe` [MkSignalEvent (MkInterval (2/3) 1) (pure ()), MkSignalEvent (MkInterval 1 (4/3)) (pure ())]

    --   it "should noop for fast 0.5" $ do
    --     (fast 0.5 pat) (MkInterval 0 1) `shouldBe` [MkSignalEvent (MkInterval 0 2) (pure ())]
    --     (fast 0.5 pat) (MkInterval 0 2) `shouldBe` [MkSignalEvent (MkInterval 0 2) (pure ())]

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
