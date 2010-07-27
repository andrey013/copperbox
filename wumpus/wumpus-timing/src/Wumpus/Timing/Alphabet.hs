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
-- Letter is a concrete syntax, Transition and Instruction form 
-- an abstract syntax.
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Alphabet
  (
    Letter(..)
  , Payload

  , Transition(..)
  , Instruction(..)

  ) where


import Wumpus.Timing.Width

import Wumpus.Core                              -- package: wumpus-core

data Letter = H Width                 -- High
            | L Width                 -- Low
            | Z Width                 -- High impendence
            | X Width                 -- Undefined
            | D Width  Payload        -- Data
            | U Width                 -- Unknown data
            | C Int    Width1         -- Clock - square wave - Int is count
            | M Width                 -- Metastasis
            | G                       -- Glitch
            | S Width                 -- Space
  deriving Show

type Payload = String


data Transition = T_Zero       -- H/H, H/S, L/L, G/any, H/Clock
                | HM           -- H/Z, H/X, H/D, H/U
                | HL           -- H/L, H/T
                | MH           -- Z/H
                | MHC          -- Z/C          -- clock pulse always high first
                | ML           -- Z/L, X/L
                | LH           -- L/H, L/T
                | LHC          -- L/H Clock
                | LM           -- L/Z, L/X
  deriving (Eq,Ord,Show)


-- OData is data followed by a space - this data does not draw
-- /properly/. That is the right-hand outline does not draw the
-- camfer.
--
-- Note Line colour is stroke colour, OData & CData colour is 
-- fill colour. OData & CData will reset the stroke colour to 
-- black.
--
data Instruction = Line         Width   DRGB
                 | CData        Width   DRGB    (Maybe Payload)
                 | OData        Width   DRGB    (Maybe Payload)
                 | Clock        Int     Width1
                 | Metastasis   Width
                 | Space        Width 
                 | Glitch
  deriving (Eq,Show)
                