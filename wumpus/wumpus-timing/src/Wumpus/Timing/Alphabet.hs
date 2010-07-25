{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Alphabet
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Syntax of timing diagrams
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Alphabet
  (
    Alphabet(..)

  ) where


import Wumpus.Timing.Width

data Alphabet = H Width                 -- High
              | L Width                 -- Low
              | Z Width                 -- High impendence
              | U Width                 -- Undefined
              | D Width  Payload        -- Data
              | C Width1                -- Clock
              | T Width1                -- Toggle
              | M Width                 -- Metastasis
              | G                       -- Glitch
              | S Width                 -- Space
  deriving Show

type Payload = String