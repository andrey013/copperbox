{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.DocTextZero
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Flexible text type, composable with @pretty-print@ style 
-- operators.
-- 
-- Direction zero (left-to-right) only.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.DocTextZero
  ( 

    Doc 
  , TextFrame

  , leftAlign
  , centerAlign
  , rightAlign

  , blank
  , space
  , string
  , escaped
--  , int
--  , integer
  , (<>)
  , (<+>) 

  , lfill
  , rfill
  , centerfill

  , fontColour
  , textSize

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Char ( ord )



-- | Space is the width of a space in the current font - it is 
-- filled in during interpretation.
--
data Doc u = Empty
           | Space 
           | Text  EscapedText
           | Cat   (Doc u)          (Doc u)
           | Fill  VAlign           u         (Doc u)
           | Local DrawingContextF  (Doc u)

-- | TextFrame is the result Graphic made from rendering multiple
-- lines of DocText.
--
type TextFrame u = BoundedLocRectGraphic u


blank       :: Doc u
blank       = Empty

space       :: Doc u
space       = Space

string      :: String -> Doc u
string      = Text . escapeString

escaped     :: EscapedText -> Doc u
escaped     = Text


infixr 6 <>, <+>


-- | Concatenate two DocTexts separated with no spacing.
--
(<>) :: Doc u -> Doc u -> Doc u
a <> b = Cat a b
 

-- | Concatenate two DocTexts separated with a space.
--
(<+>) :: Doc u -> Doc u -> Doc u
a <+> b = a <> space <> b 


rfill :: u -> Doc u -> Doc u
rfill = Fill VLeft

lfill :: u -> Doc u -> Doc u
lfill = Fill VRight


centerfill :: u -> Doc u -> Doc u
centerfill = Fill VCenter



fontColour :: RGBi -> Doc u -> Doc u
fontColour rgb = Local (text_colour rgb)


-- Note with the formulation @ CF (u,AdvGraphic u) @ changing
-- text size will not work.
--

textSize :: Int -> Doc u -> Doc u
textSize sz = Local (set_font_size sz)



leftAlign :: (Real u, Floating u, InterpretUnit u) 
          => [Doc u] -> TextFrame u
leftAlign = renderMultiLine VLeft

centerAlign :: (Real u, Floating u, InterpretUnit u) 
          => [Doc u] -> TextFrame u
centerAlign = renderMultiLine VCenter


rightAlign :: (Real u, Floating u, InterpretUnit u) 
          => [Doc u] -> TextFrame u
rightAlign = renderMultiLine VRight



renderMultiLine :: (Real u, Floating u, InterpretUnit u) 
                => VAlign -> [Doc u] -> BoundedLocRectGraphic u
renderMultiLine va docs = body >>= posTextWithMargins
  where
    body = (\dy -> let xs = map interpret docs
                   in valignSepPO emptyPosObject va dy $ reverse xs)
              <$> textlineSpace

interpret :: (Fractional u, Ord u, InterpretUnit u) 
          => Doc u -> PosObject u
interpret Empty             = interpEmpty
interpret Space             = interpSpace
interpret (Text esc)        = interpText esc
interpret (Cat a b)         = hcatPO (interpret a) (interpret b)
interpret (Fill va w a)     = interpFill va w (interpret a)
interpret (Local upd a)     = localizePO upd (interpret a)



interpEmpty :: InterpretUnit u => PosObject u
interpEmpty = makePosObject (pure $ Orientation 0 0 0 0) emptyLocGraphic



-- | Note - the current way of seeding the LocGraphic with the 
-- DrawingContext looks dodgy (substantial copying). 
-- 
-- Maybe EvalM should have a private, much smaller 
-- DrawingContext...
--
interpText :: InterpretUnit u => EscapedText -> PosObject u
interpText esc = 
    makePosObject (textOrientationZero esc) (escTextLine esc)
   



-- | Note - a space character is not draw in the output, instead 
-- 'space' advances the width vector by the width of a space in 
-- the current font.
--
interpSpace :: InterpretUnit u => PosObject u
interpSpace = 
    makePosObject (charOrientationZero $ CharEscInt $ ord ' ') emptyLocGraphic




interpFill :: (Fractional u, Ord u) 
           => VAlign -> u -> PosObject u -> PosObject u
interpFill VLeft   = padLeftPO
interpFill VCenter = padHorizontalPO
interpFill VRight  = padRightPO



{-

int :: InterpretUnit u => Int -> DocText u
int i = uniformSpace (map CharLiteral $ show i) mkvecQ 
  where
    mkvecQ = advanceH <$> charVector (CharLiteral '0') 
        


integer :: InterpretUnit u => Integer -> DocText u
integer i = uniformSpace (map CharLiteral $ show i) mkvecQ
  where
    mkvecQ = advanceH <$> charVector (CharLiteral '0')
    
-}



{-
--------------------------------------------------------------------------------
-- Helpers


uniformSpace :: InterpretUnit u 
             => [EscapedChar] -> Query u -> BoundedPosObject u
uniformSpace xs qx = hkernPrim $ qx >>= go xs
  where 
    go (c:cs) dx = return $ (0,c) : map (\ch -> (dx,ch)) cs
    go []     _  = return []


hkernPrim :: InterpretUnit u => Query [KernChar u] -> BoundedPosObject u
hkernPrim qks = 
    makeBoundedPosObject (qks >>= hkernOrientationZero) (lift0R1 qks >>= hkernLine)
           
-}

