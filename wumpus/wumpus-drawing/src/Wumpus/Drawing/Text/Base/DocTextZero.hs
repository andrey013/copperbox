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


    DocText
  , TextFrame

  , leftAlign
  , centerAlign
  , rightAlign

  , blank
  , space
  , string
  , int
  , integer
  , (<>)
  , (<+>) 

  , rfill
--  , lfill

  , fontColour
  , textSize

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Char ( ord )


-- I\'m not convinced this is the right type...

type DocText u = BoundedPosObject u
      



-- | TextFrame is the result Graphic made from rendering multiple
-- lines of DocText.
--
type TextFrame u = BoundedPosGraphic u


leftAlign :: (Real u, Floating u, InterpretUnit u) 
          => [DocText u] -> TextFrame u
leftAlign = renderMultiLine VLeft

centerAlign :: (Real u, Floating u, InterpretUnit u) 
          => [DocText u] -> TextFrame u
centerAlign = renderMultiLine VCenter


rightAlign :: (Real u, Floating u, InterpretUnit u) 
          => [DocText u] -> TextFrame u
rightAlign = renderMultiLine VRight




renderMultiLine :: (Real u, Floating u, InterpretUnit u) 
                => VAlign -> [DocText u] -> TextFrame u
renderMultiLine va docs = lift0R2 body >>= posTextWithMargins
  where
    body     = (\dy -> valignSepPO emptyBoundedPosObject va dy $ reverse docs)
                 <$> textlineSpace


          

-- | Build a blank DocText with no output and a 0 width vector.
--
blank :: InterpretUnit u => DocText u
blank = makeBoundedPosObject (pure $ zeroOrtt) emptyLocGraphic
  where
    zeroOrtt = Orientation 0 0 0 0




escaped :: InterpretUnit u => EscapedText -> DocText u
escaped esc = makeBoundedPosObject (textOrientationZero esc) (escTextLine esc)
 


-- | Build a DocText from a string.
-- 
-- Note the string should not contain newlines or tabs.
--
string :: InterpretUnit u => String -> DocText u
string = escaped . escapeString


-- | Note - a space character is not draw in the output, instead 
-- 'space' advances the width vector by the width of a space in 
-- the current font.
--
space :: InterpretUnit u => DocText u
space = makeBoundedPosObject qort emptyLocGraphic
  where
    qort = charOrientationZero (CharEscInt $ ord ' ')


int :: InterpretUnit u => Int -> DocText u
int i = uniformSpace (map CharLiteral $ show i) mkvecQ 
  where
    mkvecQ = advanceH <$> charVector (CharLiteral '0') 
        


integer :: InterpretUnit u => Integer -> DocText u
integer i = uniformSpace (map CharLiteral $ show i) mkvecQ
  where
    mkvecQ = advanceH <$> charVector (CharLiteral '0')
    


infixr 6 <>, <+>


-- | Concatenate two DocTexts separated with no spacing.
--
(<>) :: (Num u, Ord u) => DocText u -> DocText u -> DocText u
a <> b = a `hcatPO` b
 

-- | Concatenate two DocTexts separated with a space.
--
(<+>) :: (InterpretUnit u, Ord u) => DocText u -> DocText u -> DocText u
a <+> b = a <> space <> b 


-- | Right fill
--
rfill :: Num u => u -> DocText u -> DocText u
rfill w = extendPosBounds 0 w 0 0

{-
-- | Left fill
--
lfill :: (Num u, Ord u) => u -> DocText u -> DocText u
lfill w dt = DocText $ getDocText dt >>= \(u,gf) -> 
  if u < w then return (w, moveStart (displaceH $ w - u) gf) 
           else return (u,gf)
-}

-- A contextual version might be useful...
-- cxrfill :: Ord u => DrawingInfo u -> DocText u -> DocText u


-- Note - @fill@ combinators cf. @wl-pprint@ (but left and right) 
-- will be very useful.
--
-- Also PosObjects can be inlined in text...
--

fontColour :: RGBi -> DocText u -> DocText u
fontColour rgb = doclocal (text_colour rgb)


-- Note with the formulation @ CF (u,AdvGraphic u) @ changing
-- text size will not work.
--

textSize :: Int -> DocText u -> DocText u
textSize sz = doclocal (set_font_size sz)


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
           


-- This works but it looks flaky - as DocText is essentially a 
-- query, it has to (a) transform the environment which it is run,
-- (b) transform the LocImage part of the PosObject
--
doclocal :: DrawingContextF -> DocText u -> DocText u
doclocal upd = bimapPosObject (localize upd) (localize upd)



-- Currently incorrect - needs changing for fill...
extendPosBounds :: Num u 
                => u -> u -> u -> u -> BoundedPosObject u -> BoundedPosObject u
extendPosBounds x0 x1 y0 y1 =
    bimapPosObject (fmap $ extendOrientation x0 x1 y0 y1) (mapAns fn) 
  where
    fn (BBox (P2 llx lly) (P2 urx ury)) = let ll = P2 (llx - x0) (lly - y0) 
                                              ur = P2 (urx + x1) (ury + y1) 
                                          in BBox ll ur
       