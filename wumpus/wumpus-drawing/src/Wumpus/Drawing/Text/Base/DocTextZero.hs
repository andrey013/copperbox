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

  , lfillString
  , lfillEscaped
  , cfillString
  , cfillEscaped
  , rfillString
  , rfillEscaped


  , fontColour
  , textSize

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Char ( ord )


-- Design note - constructing /syntax/ like the internal Doc
-- type in @wl-pprint@ might allow the best API for multi-line
-- text.
--


type DocText u = BoundedPosObject u
      



-- | TextFrame is the result Graphic made from rendering multiple
-- lines of DocText.
--
type TextFrame u = BoundedLocRectGraphic u


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

--
-- Design note - there is no useful /feedback/ between the 
-- Orientation-Query and the LocImage in a PosObject, so we 
-- cannot have this more general fill combinator 
--
-- > lfill :: u -> DocText u -> DocText u
--
--

-- | Left fill string.
--
lfillString :: (Ord u, InterpretUnit u) => u -> String -> DocText u
lfillString w ss = lfillEscaped w (escapeString ss)


-- | Left fill escaped text.
--
lfillEscaped :: (Ord u, InterpretUnit u) => u -> EscapedText -> DocText u
lfillEscaped w esc = makeBoundedPosObject calcOrtt (escTextLine esc)
  where
    calcOrtt = fmap (lfillOrientation w) $ textOrientationZero esc




-- | Center fill String - i.e. add equal fill at left and right.
--
cfillString :: (Fractional u, Ord u, InterpretUnit u) 
            => u -> String -> DocText u
cfillString w ss = cfillEscaped w (escapeString ss)


-- | Center fill escaped text - i.e. add equal fill at left and 
-- right.
--
cfillEscaped :: (Fractional u, Ord u, InterpretUnit u) 
             => u -> EscapedText -> DocText u
cfillEscaped w esc = makeBoundedPosObject calcOrtt (escTextLine esc)
  where
    calcOrtt = fmap (cfillOrientation w) $ textOrientationZero esc


-- | Right fill String.
--
rfillString :: (Ord u, InterpretUnit u) => u -> String -> DocText u
rfillString w ss = rfillEscaped w (escapeString ss)


-- | Right fill escaped text
--
rfillEscaped :: (Ord u, InterpretUnit u) => u -> EscapedText -> DocText u
rfillEscaped w esc = makeBoundedPosObject calcOrtt (escTextLine esc)
  where
    calcOrtt = fmap (rfillOrientation w) $ textOrientationZero esc





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


lfillOrientation :: (Num u, Ord u) => u -> Orientation u -> Orientation u
lfillOrientation lw o@(Orientation xmin xmaj ymin ymaj) =
   if lw > w then Orientation (xmin+dx) xmaj ymin ymaj else o
  where
    w  = xmin + xmaj
    dx = lw - w 

cfillOrientation :: (Fractional u, Ord u) => u -> Orientation u -> Orientation u
cfillOrientation rw o@(Orientation xmin xmaj ymin ymaj) =
   if rw > w then Orientation (xmin+hdx) (xmaj+hdx) ymin ymaj else o
  where
    w   = xmin + xmaj
    hdx = 0.5 * (rw - w)

rfillOrientation :: (Num u, Ord u) => u -> Orientation u -> Orientation u
rfillOrientation rw o@(Orientation xmin xmaj ymin ymaj) =
   if rw > w then Orientation xmin (xmaj+dx) ymin ymaj else o
  where
    w  = xmin + xmaj
    dx = rw - w 

