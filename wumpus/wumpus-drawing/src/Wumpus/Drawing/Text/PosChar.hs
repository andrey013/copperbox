{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.PosChar
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

module Wumpus.Drawing.Text.PosChar
  ( 
    PosChar

  , posChar
  , posEscChar

  ) where

import Wumpus.Drawing.Text.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



type PosChar u = PosImage BoundingBox u

posChar :: (Fractional u, InterpretUnit u) => Char -> PosChar u
posChar = posEscChar . CharLiteral


posEscChar :: (Fractional u, InterpretUnit u) => EscapedChar -> PosChar u
posEscChar esc = 
   lift0R2 (charVector esc) >>= \wv -> 
   lift0R2 (makeOPos wv)    >>= \opos ->
   makePosImage opos (charImg wv esc)

charImg :: InterpretUnit u
        => AdvanceVec u -> EscapedChar -> LocImage BoundingBox u
charImg wv esc = promoteR1 $ \pt ->
    apply1R1 (makeBBox wv) pt  >>= \bbox -> 
    replaceAns bbox $ escText1 esc `at` pt

-- | Bounding box is baseline-left form.
--
makeBBox :: InterpretUnit u 
         => AdvanceVec u -> LocQuery u (BoundingBox u)
makeBBox (V2 w _) = promoteR1 $ \(P2 x y) ->
    capHeight            >>= \ymajor -> 
    fmap abs descender   >>= \yminor  ->
    let sw = P2 x     (y-yminor)
        ne = P2 (x+w) (y+ymajor)
    in return $ BBox sw ne


-- | Object pos is baseline-left form.
--
makeOPos :: InterpretUnit u 
         => AdvanceVec u -> Query (ObjectPos u)
makeOPos (V2 w _) = 
    capHeight            >>= \ymajor -> 
    fmap abs descender   >>= \yminor  ->
    return $ ObjectPos 0 w yminor ymajor

escText1 :: InterpretUnit u => EscapedChar -> LocGraphic u
escText1 ch = escTextLine $ wrapEscChar ch