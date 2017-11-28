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
  overlay $ do
    ctrl 1 127
    s [vmap (+(x)) | x <- palindrome [120..127]]

  overlay $ do
    ctrl 60 (80)
    s [vmap (+1), id] & tt (1/8)
    s [vmap (+x) | x <- [1..4]]
    vmap (+(-9))
    m

  -- s [i, shift 0.5, fast 2, i]

  overlay $ do
    ctrl 70 (20)
    s [vmap (+x) | x <- [0..3]] & tt (1/4)
    -- s [slow 2, i] & tt (1/16)
    s [vmap (+12)] & tt (1/1)
    shift 0.5
    m
    -- s [fast 2, fast 1] & tt (1/16)
    -- s [i, shift 0.5, shift 0.5, i] & tt (1)
    -- s [i, i, fast 2, i] & tt (1/2)



  overlay $ do
    overlay $ do
      note 7 (60)
      fast 2
      overlay $ do
        ctrl 7 0
        s [vmap (+x) | x <- [0, 3]] & tt (1/4)
        -- s [vmap (+x) | x <- [0, 1]] & tt (1/2)
      vmap (+100)
      overlay $ do
        note 0 (38)
        fast 2
      -- vmap (+10)
      -- s [shift 0.5, i]
      -- slow 1.5
      -- s [i, pmap (+7) . rep 1] & tt 2
      -- s [i, pmap (subtract 2)] & tt (1/4)
      -- s [i, pmap (subtract 5)] & tt (1/8)

    -- nhk
    overlay $ do
      note 6 (60)
      slow 4
      overlay $ do
        ctrl 6 60
        s [vmap (+x) | x <- [6, 15]] & tt (1/16)
      rep 1
      rep 2
      t [ shift 0.5 >>> rep 4, i] 2
      s [m, m]

    -- s [i, pmap (subtract 2)] & tt (1/8)
    -- s [i, pmap (subtract 5)] & tt (1/4)

    overlay $ do
      note 5 (60)
      overlay $ do
        s [ctrl 5 x | x <- [22]]
      s [pmap (+12), i]
      s [vmap (+2), i] & tt (1/4)
      s [vmap (+2), i] & tt (1/2)
      s [i, vmap (+2)] & tt (1)
      s [i, i]

    t [shift 0.5, i] 8
    -- t [fast 2, i, fast 2, i] 2

    s [pmap (+x) | x <- [0,-2]] & tt (1/16)
    -- s [pmap (+x) | x <- [0, -5]] & tt (1/4)
    -- s [pmap (+x) | x <- [0, 12]] & tt (2)
    -- pmap (+7)

    -- overlay $ do
    --   s [shift 0.5, m] & tt (4/3)
      -- s [pmap (+12), i] & tt (4/3)

  overlay $ do
    ctrl 3 127
    -- s [vmap (const x) | x <- palindrome [(127-16)..127]]
    -- s [vmap (+(x*40)) | x <- [0,-1]]
    -- fast 2
    -- s [fast 4, i]

  -- t [shift 0.5, i] 8
  -- s [pmap (+12), shift 0.5, i, shift 0.5] & tt (1)

  overlay $ do
    note 0 (40)
    -- s [i, overlay $ t [shift 0.25, i] 8]

    overlay $ do
      note 0 (47)
      shift 0.5
      slow 2

    -- t [shift 0.5, i] 4
  -- s [i, shift 1, i, shift 0.5] & tt (2)
  -- s [i, t [shift 0.5 . fast 2, i] 2]
  -- t [rep 1, i, rep 2, i] 4
