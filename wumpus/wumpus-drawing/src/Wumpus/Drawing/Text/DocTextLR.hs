{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.DocTextLR
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
-- Left-to-right only.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.DocTextLR
  ( 
   
    DocTextLR
  , leftAlign
  , centerAlign
  , rightAlign

  , blank
  , space
  , string
  , (<>)
  , (<+>) 

  , fontColour

  ) where

import Wumpus.Drawing.Chains
import Wumpus.Drawing.Text.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.Char ( ord )

-- Need to know line width (horizontal) and line count (vertical) 
-- to render...
--
-- Can obviously access line count if we avoid avoid operators 
-- for vertical composition operator and delegate it to rendering
-- instead:
--
-- > rightAlign :: [CatText u] -> PosImage u (BoundingBox u)
--

-- A CatPrim returns a drawing function (AdvGraphic) to be used
-- drawing final rendering.
--
type DocPrim u = CF (u, AdvGraphic u)

newtype DocTextLR u = DocTextLR { getDocTextLR :: JoinList (DocPrim u) }



-- | 'HMove' : @ half_max_width * line_width -> Horizontal_Displacement @
--
type HMove u = u -> u -> u

leftAMove :: Num u => HMove u
leftAMove half_max _ = negate half_max
 
centerAMove :: Fractional u => HMove u
centerAMove _ elt_w = negate $ 0.5 * elt_w

rightAMove :: Num u => HMove u
rightAMove half_max elt_w = half_max - elt_w



leftAlign :: (Real u, FromPtSize u, Floating u) 
          => [DocTextLR u] -> PosImage u (BoundingBox u)
leftAlign = drawMulti leftAMove

centerAlign :: (Real u, FromPtSize u, Floating u) 
            => [DocTextLR u] -> PosImage u (BoundingBox u)
centerAlign = drawMulti centerAMove

rightAlign :: (Real u, FromPtSize u, Floating u) 
           => [DocTextLR u] -> PosImage u (BoundingBox u)
rightAlign = drawMulti rightAMove



drawMulti :: (Real u, FromPtSize u, Floating u) 
          => HMove u -> [DocTextLR u] -> PosImage u (BoundingBox u)
drawMulti moveF xs = promoteR2 $ \start rpos -> 
    evalAllLines xs                     >>= \all_lines -> 
    centerToBaseline                    >>= \down -> 
    borderedTextPos line_count (fst all_lines) >>= \opos ->
    let chn   = centerSpinePoints line_count 0 
        gs    = positionHLines moveF down all_lines 
        gf    = unchainZip emptyLocGraphic gs chn
        posG  = makePosImage opos gf
        bbox  = objectPosBounds start rpos opos
    in replaceAns bbox $ atStartPos posG start rpos     
  where
    line_count    = length xs

positionHLines :: Fractional u 
               => HMove u -> u -> (u,[(u, AdvGraphic u)]) -> [LocGraphic u]
positionHLines mkH down (max_w,xs) = map fn xs
  where
    half_max       = 0.5 * max_w
    moveF w1       = let v = vec (mkH half_max w1) (-down) 
                     in moveStart $ displaceVec v 
    fn (elt_w, gf) = ignoreAns $ moveF elt_w $ gf


evalAllLines :: (Num u, Ord u) 
             => [DocTextLR u] -> DrawingInfo (u, [(u, AdvGraphic u)])
evalAllLines = fmap post . mapM evalLine
  where
    post xs = let mx = foldr (\(a,_) x -> max a x) 0 xs in (mx,xs)
            




evalLine :: Num u => DocTextLR u -> DrawingInfo (u, AdvGraphic u)
evalLine ct = case viewl $ getDocTextLR ct of
    EmptyL -> return (0,  replaceAns (hvec 0) $ emptyLocGraphic)
    af :< rest -> af >>= \a -> go a (viewl rest)
  where
    go acc     EmptyL     = return acc
    go (dx,af) (mf :< ms) = let moveF = moveStart (displaceH dx)
                            in mf >>= \(u,gf) -> 
                               go (dx+u, af `oplus` moveF gf) (viewl ms)





-- | Build a blank DocTextLR with no output and a 0 width vector.
--
blank :: Num u => DocTextLR u
blank = doc1 $ return (0, replaceAns (hvec 0) $ emptyLocGraphic)

-- | Note - a space character is not draw in the output, instead 
-- 'space' advances the width vector by the width of a space in 
-- the current font.
--
space :: FromPtSize u => DocTextLR u
space = doc1 $ 
   charVector (CharEscInt $ ord ' ') >>= \v -> 
   return (advanceH v, replaceAns v $ emptyLocGraphic)

-- | Build a DocTextLR from a string.
--
string :: FromPtSize u => String -> DocTextLR u
string = doc1 . stringPrim


infixr 6 <>, <+>

-- | Concatenate two DocTextLRs separated with no spacing.
--
(<>) :: DocTextLR u -> DocTextLR u -> DocTextLR u
a <> b = DocTextLR $ JL.join (getDocTextLR a) (getDocTextLR b) 


-- | Concatenate two DocTextLRs separated with a space.
--
(<+>) :: FromPtSize u => DocTextLR u -> DocTextLR u -> DocTextLR u
a <+> b = a <> space <> b 



-- Note - @fill@ combinators cf. @wl-pprint@ (but left and right) 
-- will be very useful.
--
-- Also PosImages can be inlined in text...
--

doc1 :: DocPrim u -> DocTextLR u
doc1 = DocTextLR . JL.one 


stringPrim :: FromPtSize u => String -> DocPrim u
stringPrim = escapedPrim . escapeString

escapedPrim :: FromPtSize u => EscapedText -> DocPrim u
escapedPrim esc = textVector esc >>= \v -> 
                  return (vector_x v, replaceAns v $ escapedline esc)


docMap :: (AdvGraphic u -> AdvGraphic u) -> DocTextLR u -> DocTextLR u
docMap f = DocTextLR . fmap (fmap (\(u,ag) -> (u, f $ ag))) . getDocTextLR

doclocal :: DrawingContextF -> DocTextLR u -> DocTextLR u
doclocal fn = docMap (localize fn)


fontColour :: RGBi -> DocTextLR u -> DocTextLR u
fontColour rgb = doclocal (textColour rgb)


