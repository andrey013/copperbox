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
  , lfill

  , fontColour
  , textSize

  ) where

import Wumpus.Drawing.Chains
import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.Char ( ord )
import Data.Foldable ( foldrM )

-- Need to know line width (horizontal) and line count (vertical) 
-- to render...
--
-- Can obviously access line count if we avoid operators for 
-- vertical composition operator and delegating to the rendering
-- instead:
--
-- > rightAlign :: [CatText u] -> PosImage BoundingBox u
--


newtype DocText u = DocText { getDocText :: Query (u, AdvGraphic u) }

-- Note being able to background fill would be very useful, 
-- especially background fill with white.



-- | 'HMove' : @ half_max_width * line_width -> Horizontal_Displacement @
--
type HMove u = u -> u -> u

leftAMove :: Num u => HMove u
leftAMove half_max _ = negate half_max
 
centerAMove :: Fractional u => HMove u
centerAMove _ elt_w = negate $ 0.5 * elt_w

rightAMove :: Num u => HMove u
rightAMove half_max elt_w = half_max - elt_w



leftAlign :: (Real u, Floating u, InterpretUnit u) 
          => [DocText u] -> PosImage BoundingBox u
leftAlign = drawMulti leftAMove

centerAlign :: (Real u, Floating u, InterpretUnit u) 
            => [DocText u] -> PosImage BoundingBox u
centerAlign = drawMulti centerAMove

rightAlign :: (Real u, Floating u, InterpretUnit u) 
           => [DocText u] -> PosImage BoundingBox u
rightAlign = drawMulti rightAMove



drawMulti :: (Real u, Floating u, InterpretUnit u) 
          => HMove u -> [DocText u] -> PosImage BoundingBox u
drawMulti moveF xs = promoteR2 $ \start rpos -> 
    evalAllLines xs                             >>= \all_lines -> 
    centerToBaseline                            >>= \down -> 
    borderedTextPos line_count (fst all_lines)  >>= \opos ->
    centerSpineDisps line_count 0               >>= \(disp_top, disp_next) ->
    let gs    = positionHLines moveF down all_lines 
        gf    = moveStart disp_top $ chainDisplace disp_next gs
        posG  = makePosImage opos gf
        bbox  = objectPosBounds start rpos opos
    in replaceAns bbox $ atStartPos posG start rpos     
  where
    line_count    = length xs

    -- chain is many graphics 


positionHLines :: Fractional u 
               => HMove u -> u -> (u,[(u, AdvGraphic u)]) -> [LocGraphic u]
positionHLines mkH down (max_w,xs) = map fn xs
  where
    half_max       = 0.5 * max_w
    moveF w1       = let v = vec (mkH half_max w1) (-down) 
                     in moveStart $ displaceVec v 
    fn (elt_w, gf) = ignoreAns $ moveF elt_w $ gf


evalAllLines :: (Num u, Ord u) 
             => [DocText u] -> Query (u, [(u, AdvGraphic u)])
evalAllLines = foldrM fn (0,[]) 
  where
    fn dt (maxw,xs) = getDocText dt >>= \ans@(dx,_) -> 
                      return (max dx maxw, ans:xs)


-- | Build a blank DocText with no output and a 0 width vector.
--
blank :: InterpretUnit u => DocText u
blank = DocText $ return (0, replaceAns (hvec 0) $ emptyLocGraphic)




escaped :: InterpretUnit u => EscapedText -> DocText u
escaped esc = DocText $ body 
   where
     body = textVector esc >>= \v -> 
            return (vector_x v, replaceAns v $ escTextLine esc)


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
   charVector (CharEscInt $ ord ' ') >>= \v -> 
   return (advanceH v, replaceAns v $ emptyLocGraphic)





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
(<>) :: Num u => DocText u -> DocText u -> DocText u
a <> b = DocText body 
  where 
    body = getDocText a >>= \(dx0,gf0) -> 
           getDocText b >>= \(dx1,gf1) -> 
           return (dx0 + dx1, gf0 `oplus` moveStart (displaceH dx0) gf1)  


-- | Concatenate two DocTexts separated with a space.
--
(<+>) :: InterpretUnit u => DocText u -> DocText u -> DocText u
a <+> b = a <> space <> b 

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


-- A contextual version might be useful...
-- cxrfill :: Ord u => DrawingInfo u -> DocText u -> DocText u


-- Note - @fill@ combinators cf. @wl-pprint@ (but left and right) 
-- will be very useful.
--
-- Also PosImages can be inlined in text...
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
             => u -> [EscapedChar] -> Query (u,AdvGraphic u)
uniformSpace dx xs = hkernPrim $ go xs
  where 
    go (c:cs) = (0,c) : map (\ch -> (dx,ch)) cs
    go []     = []


hkernPrim :: InterpretUnit u => [KernChar u] -> Query (u,AdvGraphic u)
hkernPrim ks = hkernVector ks >>= \v ->
               uconvertCtxF v  >>= \v1 -> 
               return (vector_x v1, replaceAns v1 $ hkernLine ks)


-- | Note - the changes to the DrawingContext have to be 
-- propagated both to the function that generates the answer and 
-- to the AdvGraphic of result.
--
doclocal :: DrawingContextF -> DocText u -> DocText u
doclocal fn dt = DocText $ 
    localize fn $ getDocText dt >>= \(u,gf) -> return (u, localize fn gf)



