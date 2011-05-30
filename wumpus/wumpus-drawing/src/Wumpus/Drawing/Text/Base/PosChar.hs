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
-- Draw a single char as a PosObject or LocRectImage.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.PosChar
  ( 

    LocRectChar
  , PosChar

  , charLabel
  , escCharLabel

  , charLabelUp
  , escCharLabelUp

  , posChar
  , posEscChar

  , posCharUp
  , posEscCharUp

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core



-- | Functional type from 'RectAddress' to LocImage.
--
-- This is the simpler type for just drawing single Chars at some
-- rectangular position (e.g. CENTER, BLL - baseline left, etc.).
--
type LocRectChar u = RectAddress -> LocImage u (BoundingBox u)


-- | Alias for 'PosObject'. 
-- 
-- 'PosChar' is more versatile than 'LocRectChar' (it supports 
-- concatentation etc.), but consequently it has a more 
-- complicated API.
-- 
type PosChar u = PosObject u





--------------------------------------------------------------------------------

-- | Make a 'LocRectChar' (i.e. a /label/) from a Char.
--
-- Note - the vertical height is measured from the descender-depth 
-- to the cap-height. 
-- 
charLabel :: (Floating u, InterpretUnit u) => Char -> LocRectChar u
charLabel ch = escCharLabel $ CharLiteral ch


-- | Make a 'LocRectChar' (i.e. a /label/) from an 'EscapedChar'.
--
-- Note - the vertical height is measured from the descender-depth 
-- to the cap-height. 
-- 
escCharLabel :: (Floating u, InterpretUnit u) 
             => EscapedChar -> LocRectChar u
escCharLabel esc = \raddr -> 
    runPosObject raddr (makePosChar CAP_HEIGHT_PLUS_DESCENDER esc) 



-- | /Upright/ version of 'charLabel'.
--
-- The vertical orientation is calculated using only the 
-- cap-height and ignoring the descender depth.
--
-- In circumstances where you want visually good vertical 
-- centering and know the input char has no descender 
-- (e.g. digits) use this rather than 'charLabel'.
--
charLabelUp :: (Floating u, InterpretUnit u) => Char -> LocRectChar u
charLabelUp ch = escCharLabel $ CharLiteral ch


-- | /Upright/ version of 'escCharLabel'.
--
escCharLabelUp :: (Floating u, InterpretUnit u) 
               => EscapedChar -> LocRectChar u
escCharLabelUp esc = \raddr -> 
    runPosObject raddr (makePosChar JUST_CAP_HEIGHT esc) 






-- | Make a 'PosObject' from a single Char.
--
posChar :: InterpretUnit u => Char -> PosChar u
posChar = makePosChar CAP_HEIGHT_PLUS_DESCENDER . CharLiteral

-- | Make a 'PosObject' from a single 'EscapedChar'.
--
posEscChar :: InterpretUnit u => EscapedChar -> PosChar u
posEscChar = makePosChar CAP_HEIGHT_PLUS_DESCENDER




-- | /Upright/ version of 'posChar'.
--
-- The vertical orientation is calculated using only the 
-- cap-height and ignoring the descender depth.
--
-- In circumstances where you want visually good vertical 
-- centering and know the input char has no descender 
-- (e.g. digits) use this rather than 'posChar'.
--
posCharUp :: InterpretUnit u => Char -> PosChar u
posCharUp = makePosChar JUST_CAP_HEIGHT . CharLiteral


-- | /Upright/ version of 'posEscChar'.
--
posEscCharUp :: InterpretUnit u => EscapedChar -> PosChar u
posEscCharUp = makePosChar CAP_HEIGHT_PLUS_DESCENDER


-- | Helper.
--
makePosChar :: InterpretUnit u 
            => TextVSize -> EscapedChar -> PosObject u
makePosChar vsz esc = 
    makePosObject (charOrientationZero vsz esc) 
                  (dcEscapedlabel $ wrapEscChar esc)




