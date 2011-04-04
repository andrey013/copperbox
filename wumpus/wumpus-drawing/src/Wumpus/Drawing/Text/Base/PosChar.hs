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
    LocRectChar

  , posChar
  , posEscChar

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



-- Note this type doesn\'t support concat...
-- 
-- While it may be adequate, it does need another prefix.
--
type LocRectChar u = BoundedLocRectGraphic u

posChar :: (Floating u, InterpretUnit u) => Char -> LocRectChar u
posChar ch = posEscChar $ CharLiteral ch


posEscChar :: (Floating u, InterpretUnit u) 
           => EscapedChar -> LocRectChar u
posEscChar esc = 
    promoteR2 $ \pt addr -> runPosObject pt addr (makePosChar esc) 


makePosChar :: InterpretUnit u 
            => EscapedChar -> PosObject u
makePosChar esc = makePosObject (charOrientationZero esc) (escText1 esc)


escText1 :: InterpretUnit u => EscapedChar -> LocGraphic u
escText1 ch = escTextLine $ wrapEscChar ch



