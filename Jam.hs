{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Jam where

import Prelude
import Control.Arrow ((>>>))
import Control.Concurrent
import Data.Function ((&))
import Util
import Syzygy.Signal
import Syzygy.Base
import Syzygy.Core

main :: IO ()
main = do
  MkCoreConfig {signalRef, bpmRef} <- runOnce setup
  modifyMVar_ bpmRef $ const . return $ 160
  modifyMVar_ signalRef $ const . return $ mempty
    & sigMod

sigMod :: Signal ParamMap -> Signal ParamMap
sigMod = let (>>) = (flip (.)) in do
  id
  id

  let 
    sweep n = do
      s [ctrl 3 (x) | x <- palindrome [(127-n)..127]]
      add $ do
        s [ctrl 1 (x) | x <- palindrome [(127-n)..127]]

  add $ do
    -- A
    add $ do
      note 8 (60)
      slow 8
      -- t 1 [shift 0.25, fast 2]
      add $ do
        ctrl 8 0
        t 1 [vmap (+x) | x <- [0, 3]]
        s [vmap (+(x*1)) | x <- [0..15]] & tt (1)
      vmap (+10)

      -- t 0.5 [vmap (+70), i]
      -- t 8 [pmap (+12), i]
      -- add $ do
      --   note 0 (12)
        -- t 2 [fast 4, fast 2]
      -- s [fast 2, shift 0.25]
      -- s [fast 2, m]
      -- t (1/3) [i, m] 

    -- B
    add $ do
      note 6 (60)
      t 8 [fast 4 . stac 4]
      -- t 2 [i, pmap (+12)]
      -- t 4 [pmap (+12), shift 0.5]
      add $ do
        ctrl 6 0
        -- t (1) [vmap (+x) | x <- [0, 32]]
      t 8 [vmap (+(x*1)) | x <- [0..32]]
      -- vmap (+20)
      -- shift 0.5
      -- slow 2
      t (1/3) [i, m] 
      m
      -- t 1 [i, rep 2]
      -- t 2 [i, shift 0.5]

  -- t 2 [pmap (+12), i, i]
  -- t 16 [i, t (1/4) [shift 0.5 . pmap (+12) . t 2 [i, fast 2], m]]
  -- t 1 [i, pmap (+7)]
  -- t 2 [i, pmap (+12)]
  -- t 2 [rep 2, rep 4]

  add $ do
    sweep 0
    -- fast 4
    -- s [vmap (subtract (x*1)) | x <- [0, 80]] >>> fast 2
    shift 0.5
    -- fast 2
    -- vmap (const 120)
    -- t 8 [fast 2, i]
    -- shift 0.5
    -- fast 2
    -- fast 4

    -- t 16 [vmap (subtract 40), i]
    -- t 1 [rep 4, shift 0.25]
    -- t 1 [i, shift 0.25]

  add $ do
    m
    add $ do
      s [note 0 x | x <- [1]]
      s [fast 2, i]
      shift 0.5

    add $ do
      s [note 0 x | x <- [14,15]]

    t 1 [i,  add $ pmap (+6) . fast 2]
    m

    -- s [i, shift 0.25]
    -- overlay $ do
    --   shift 0.5
  -- t (1/3) [rep 1, shift 0.5]
  -- t 1 [i, rep 4]

  -- add $ do
  --   s [note 0 (x*12) | x <- [0]]

  -- add $ do
  --   s [note 1 (x*2+60) | x <- [0..12]]
  --   add $ do
  --     pmap (+12)
  --     slow 1.13

  --   s [i, shift 0.7]
  --   add $ shift 0.25

  -- t 8 (palindrome [slow 8, slow 4, slow 2, i])
  -- add $ do
  --   s [note 0 x | x <- [0]]

  -- t 1 [i, shift 0.25 . t 2 [rep 2, s[pmap (+12), rep 4]]]
