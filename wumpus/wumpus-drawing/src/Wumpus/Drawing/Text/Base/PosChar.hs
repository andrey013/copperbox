{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.PosChar
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Drawing single a char as a PosImage.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.PosChar
  ( 
    PosChar
  , LocRectChar

  , posChar
  , posEscChar

  , charLabel
  , escCharLabel

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


type PosChar u = PosObject u

posChar :: InterpretUnit u => Char -> PosChar u
posChar = makePosChar . CharLiteral

posEscChar :: InterpretUnit u => EscapedChar -> PosChar u
posEscChar = makePosChar



-- Note this type doesn\'t support concat...
-- 
-- While it may be adequate, it does need another prefix.
--
type LocRectChar u = RectAddress -> LocImage u (BoundingBox u)




charLabel :: (Floating u, InterpretUnit u) => Char -> LocRectChar u
charLabel ch = escCharLabel $ CharLiteral ch


escCharLabel :: (Floating u, InterpretUnit u) 
             => EscapedChar -> LocRectChar u
escCharLabel esc = \raddr -> runPosObject raddr (makePosChar esc) 


makePosChar :: InterpretUnit u 
            => EscapedChar -> PosObject u
makePosChar esc = makePosObject (charOrientationZero esc) (escText1 esc)


escText1 :: InterpretUnit u => EscapedChar -> LocGraphic u
escText1 ch = dcEscapedlabel $ wrapEscChar ch



