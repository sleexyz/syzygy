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
    overlay $ do
    -- A
      note 7 (60)
      fast 1
      overlay $ do
        ctrl 7 0
        s [vmap (+x) | x <- [0, 3]] & tt (1/4)
        -- s [vmap (+x) | x <- [0, 1]] & tt (1/2)
      vmap (+100)
      -- pmap (+12)
      overlay $ do
        note 0 (38)
        fast 2
      -- vmap (+10)
      s [i, slow 1.5] & tt 2
      s [m, m]

    -- B
    overlay $ do
      note 5 (60)
      fast 4
      overlay $ do
        s [ctrl 5 x | x <- [19]]
      s [pmap (+12), i] & tt 1
      s [vmap (+2), i] & tt (1/4)
      s [vmap (+2), i] & tt (1/2)
      s [i, vmap (+2)] & tt (1)
      t [s [i, m] & tt 2, i] 4
      vmap (+(-1))
      t [m, m] 1

    -- C
    overlay $ do
      note 6 (60)
      slow 1
      overlay $ do
        ctrl 6 60
        s [vmap (+x) | x <- [15, 45]] & tt (1/8)
      rep 1
      rep 2
      -- t [shift 0.5 >>> rep 4, i] 4
      -- shift 0.5
      s [m, m]

    -- D
    overlay $ do
      note 8 (60)
      s [fast 1, fast 4, fast 1, fast 4] 
      slow 4
      overlay $ do
        ctrl 8 0
        s [vmap (+x) | x <- [15, 16]] & tt (1/2)
        s [vmap (+20), vmap (+30)] & tt (1/8)
      rep 1
      rep 2
      t [shift 0.5 >>> rep 4, i] 4
      s [pmap (+12), pmap (+12), pmap (+12)] & tt 1
      s [i, i]


    -- t [shift 0.5, i] 8
    -- t [fast 2, i, fast 2, i] 2

    -- s [pmap (+x) | x <- [0,-2]] & tt (1/16)
    -- s [pmap (+x) | x <- [0, -5]] & tt (1/4)
    -- s [pmap (+x) | x <- [0, 12]] & tt (2)
    -- pmap (+7)

    -- overlay $ do
    --   s [shift 0.5, m] & tt (4/3)
      -- s [pmap (+12), i] & tt (4/3)

  overlay $ do
    ctrl 3 127
    s [vmap (const x) | x <- palindrome [(127-16)..127]]
    overlay $ do
      ctrl 1 127
      s [vmap (+(x)) | x <- palindrome [20..127]]
    -- s [vmap (+(x*10)) | x <- [0,-10]]  
    -- s [fast 8, fast 4]
    -- s [fast 2, i] & tt (2/3)
    -- fast 2
    -- s [fast 2, i]
    -- fast 2

  -- t [shift 0.5, i] 8
  -- s [pmap (+12), shift 0.5, i, shift 0.5] & tt (1)

  overlay $ do
    note 0 (38)
    shift 0.5
    -- t [fast 4, shift 0.5, fast 2, fast 1] 1
    overlay $ do
      note 0 (42)
      shift 0.5
      -- fast 2
    -- t [shift 0.5, i] 2
    -- t [fast 4, shift 0.5, fast 2, fast 1] 1
    -- t [fast 2, overlay $ t [shift 0.25, i] 8] 2
    -- s [pmap (+2)] & tt (1/32) -- bon
    -- s [pmap (+5), i] & tt (4) -- wn
    overlay $ do
      note 0 (40)

  t [shift 0.5, i] 2

    -- slow 2

  -- t [slow 2, i] 4

  -- t [shift 0.5, i] 2

  -- t [i, t [rep 2, s[pmap (+12), rep 4]] 2 ] 4

    -- overlay $ do
    --   note 0 (47)
    --   shift 0.5
    --   fast 4


  -- t [shift 0.5, i] 4
  -- t [shift 0.5, i] 2
  -- t [shift 0.5, i] 1

    -- t [shift 0.5, i] 4
  -- s [i, shift 1, i, shift 0.5] & tt (2)
  -- s [i, t [shift 0.5 . fast 2, i] 2]
  -- t [rep 1, i, rep 2, i] 4
