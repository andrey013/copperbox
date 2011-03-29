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

--  , rfill
--  , lfill

  , fontColour
  , textSize


  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Char ( ord )


newtype DocText u = DocText { getDocText :: Query (BoundedPosObject u) }




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
renderMultiLine va ds = 
    promoteR2 $ \pt addr -> body >>= \a -> runPosObject pt addr a
  where
    runDocs  = sequence . reverse . map getDocText
    body     = (\docs dy -> valignSepPO emptyBoundedPosObject va dy docs)
                 <$> runDocs ds  <*> textlineSpace
          
    




-- | Build a blank DocText with no output and a 0 width vector.
--
blank :: InterpretUnit u => DocText u
blank = DocText $ return $ makeBoundedPosObject zeroOrtt emptyLocGraphic
  where
    zeroOrtt = Orientation 0 0 0 0




escaped :: InterpretUnit u => EscapedText -> DocText u
escaped esc = DocText $ 
    (\ortt -> makeBoundedPosObject ortt (escTextLine esc))
      <$> textOrientationZero esc


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
space = DocText $ 
     (\ortt -> makeBoundedPosObject ortt emptyLocGraphic)
        <$> charOrientationZero (CharEscInt $ ord ' ')


int :: InterpretUnit u => Int -> DocText u
int i = DocText $ 
    charVector (CharLiteral '0') >>= \v1 -> 
    uniformSpace (advanceH v1) (map CharLiteral $ show i)


integer :: InterpretUnit u => Integer -> DocText u
integer i = DocText $ 
    charVector (CharLiteral '0') >>= \v1 -> 
    uniformSpace (advanceH v1) (map CharLiteral $ show i)



infixr 6 <>, <+>


-- | Concatenate two DocTexts separated with no spacing.
--
(<>) :: (Num u, Ord u) => DocText u -> DocText u -> DocText u
a <> b = DocText body 
  where 
    body = hcatPO <$> getDocText a <*> getDocText b
 

-- | Concatenate two DocTexts separated with a space.
--
(<+>) :: (InterpretUnit u, Ord u) => DocText u -> DocText u -> DocText u
a <+> b = a <> space <> b 

{-
-- | Right fill
--
rfill :: Ord u => u -> DocText u -> DocText u
rfill w dt = DocText $ getDocText dt >>= \(u,gf) -> return (max w u, gf)


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
             => u -> [EscapedChar] -> Query (BoundedPosObject u)
uniformSpace dx xs = hkernPrim $ go xs
  where 
    go (c:cs) = (0,c) : map (\ch -> (dx,ch)) cs
    go []     = []


hkernPrim :: InterpretUnit u => [KernChar u] -> Query (BoundedPosObject u)
hkernPrim ks = (\ortt -> makeBoundedPosObject ortt (hkernLine ks))
                 <$> hkernOrientationZero ks

-- Cannot have doc local anymore...
--

-- | Note - the changes to the DrawingContext have to be 
-- propagated both to the function that generates the answer and 
-- to the AdvGraphic of result.
--
doclocal :: DrawingContextF -> DocText u -> DocText u
doclocal upd a = DocText $ localize upd (getDocText a)


