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



type PosChar u = PosImage u (BoundingBox u)

posChar :: (Fractional u, PtSize u) => Char -> PosChar u
posChar = posEscChar . CharLiteral


posEscChar :: (Fractional u, PtSize u) => EscapedChar -> PosChar u
posEscChar esc = 
   lift0R2 (charVector esc) >>= \wv -> 
   lift0R2 (makeOPos wv)    >>= \opos ->
   makePosImage opos (charImg wv esc)

charImg :: PtSize u 
        => AdvanceVec u -> EscapedChar -> LocImage u (BoundingBox u)
charImg wv esc = 
    makeBBox wv >>= \bbox -> 
    replaceAns bbox $ escText1 esc

-- | Bounding box is baseline-left form.
--
makeBBox :: PtSize u 
     => AdvanceVec u -> LocDrawingInfo u (BoundingBox u)
makeBBox (V2 w _) = promoteR1 $ \(P2 x y) ->
    glyphCapHeight            >>= \ymajor -> 
    fmap abs glyphDescender   >>= \yminor  ->
    let sw = P2 x     (y-yminor)
        ne = P2 (x+w) (y+ymajor)
    in return $ BBox sw ne


-- | Object pos is baseline-left form.
--
makeOPos :: PtSize u 
         => AdvanceVec u -> DrawingInfo (ObjectPos u)
makeOPos (V2 w _) = 
    glyphCapHeight            >>= \ymajor -> 
    fmap abs glyphDescender   >>= \yminor  ->
    return $ ObjectPos 0 w yminor ymajor

escText1 :: PtSize u => EscapedChar -> LocGraphic u
escText1 ch = escapedline $ wrapEscChar ch