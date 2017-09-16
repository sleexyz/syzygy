module TestUtils where

import Data.Monoid
import Test.Hspec

import qualified Test.Hspec.Expectations

shouldBeLessThan :: (HasCallStack, Show a, Ord a) => a -> a -> IO ()
shouldBeLessThan x y =
  if x < y
  then return ()
  else Test.Hspec.Expectations.expectationFailure
    $ "expected "
    <> show x
    <> " to be less than "
    <> show y
