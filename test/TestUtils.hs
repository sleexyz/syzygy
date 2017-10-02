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

mean :: Fractional a => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

doUntil :: (a -> Bool) -> IO a -> IO a
doUntil pred action = do
  result <- action
  if pred result
  then return result
  else doUntil pred action
