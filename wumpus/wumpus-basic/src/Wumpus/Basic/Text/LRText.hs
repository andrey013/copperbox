{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.LRText
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Left-to-right measured text. The text uses glyph metrics so it 
-- can be positioned accurately.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.LRText
  ( 

   
    singleLRText

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight

  , lineStartPoints

  ) where


import Wumpus.Basic.Graphic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Char
import Data.Foldable ( foldrM )
import qualified Data.Map               as Map
import Data.Maybe 




singleLRText :: (Ord u, FromPtSize u) 
             => InterimText1 u -> BoundedLocGraphic u
singleLRText (InterimText1 esc av) =
    glyphHeightRange >>= \(ymin, ymax)  ->
    promote1 $ \pt -> let w   = advanceH av
                          ll  = pt .+^ vvec ymin
                          ur  = pt .+^ vec w ymax
                          bb  = boundingBox ll ur 
                      in (escapedline esc `at` pt) >>= \prim ->
                         return (bb, prim)


multiAlignLeft      :: (Fractional u, Ord u, FromPtSize u) 
                    => String -> BoundedLocGraphic u
multiAlignLeft      = 
    multiAligned drawLeftAligned1 (\wv pt -> pt .-^ hvec (0.5 * advanceH wv))


multiAlignCenter    :: (Fractional u, Ord u, FromPtSize u) 
                    => String -> BoundedLocGraphic u
multiAlignCenter    = 
    multiAligned drawCenterAligned1 (\_ pt -> pt)

multiAlignRight     :: (Fractional u, Ord u, FromPtSize u) 
                    => String -> BoundedLocGraphic u
multiAlignRight     = 
    multiAligned drawRightAligned1 (\wv pt -> pt .+^ hvec (0.5 * advanceH wv))


-- Note needs sorting so that empty ss is harmless.

multiAligned :: (Fractional u, Ord u, FromPtSize u) 
             => (InterimText1 u -> Point2 u -> BoundedGraphic u)
             -> (AdvanceVec u -> Point2 u -> Point2 u)
             -> String -> BoundedLocGraphic u
multiAligned drawF dispF ss = 
    linesToInterims ss >>= \(wv,xs)   ->
    glyphCapHeight     >>= \cap_h     -> 
    baselineSpacing    >>= \base_span ->   
    promote1 $ \p0 -> let p1  = dispF wv p0
                          axs = annotateStartPoints cap_h base_span p1 xs
                      in mergeLines drawF p1 axs

-- This needs sorting out so as not to throw an error
--
mergeLines :: (Num u, Ord u)
           => (InterimText1 u -> Point2 u -> BoundedGraphic u)
           -> Point2 u
           -> [(InterimText1 u, Point2 u)] -> BoundedGraphic u 
mergeLines fn fallback_pt = step
  where
    step [(x,pt)]    = fn x pt
    step ((x,pt):ys) = fn x pt `oplus` step ys  
    step _           = fn empty_fallback fallback_pt 

    empty_fallback   = InterimText1 (escapeString "") (hvec 0)


-- This isn't worth the complexity  to get to one traversal...

linesToInterims :: (FromPtSize u, Ord u) 
                => String -> DrawingInfo (AdvanceVec u, [InterimText1 u])
linesToInterims = fmap post . mapM interimText . lines
  where
    post xs   = let vmax = foldr fn (hvec 0) xs in (vmax,xs)
    fn a vmax = avMaxWidth (text1_advance a) vmax

avMaxWidth :: Ord u => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
avMaxWidth a@(V2 w1 _) b@(V2 w2 _) = if w2 > w1 then b else a

-- Point is baseline-left
--
drawLeftAligned1 :: (Ord u, FromPtSize u) 
                 => InterimText1 u -> Point2 u -> BoundedGraphic u
drawLeftAligned1 itext pt = singleLRText itext `at` pt

-- Point is baseline-center
--
drawCenterAligned1 :: (Fractional u, Ord u, FromPtSize u)
                   => InterimText1 u -> Point2 u -> BoundedGraphic u
drawCenterAligned1 itext pt = 
    let hw = 0.5 * advanceH (text1_advance itext)    
    in singleLRText itext `at` (pt .-^ hvec hw)
   


-- Point is baseline-right
--
drawRightAligned1 :: (Fractional u, Ord u, FromPtSize u)
                  => InterimText1 u -> Point2 u -> BoundedGraphic u
drawRightAligned1 itext pt = 
    let w = advanceH (text1_advance itext)
    in singleLRText itext `at` (pt .-^ hvec w)

-- One line of multiline text
--
data InterimText1 u = InterimText1
      { text1_escaped :: EscapedText
      , text1_advance :: (AdvanceVec u)
      } 
  deriving (Eq,Show)


interimText :: FromPtSize u => String -> DrawingInfo (InterimText1 u)
interimText ss = let esc = escapeString ss in 
    postpro (\wvec -> InterimText1 esc wvec) $ textVector esc


{-
-- | Measured text box for left-to-right text.
-- 
-- Supplied point is baseline left. 
-- @ymin@ is expected to be negative.
-- 
measuredTextBBox :: (Num u, Ord u) => u -> (u,u) -> Point2 u -> BoundingBox u
measuredTextBBox w (ymin,ymax) (P2 x y) = 
    boundingBox (P2 x (y+ymin)) (P2 (x+w) (y+ymax))
-}


textVector :: FromPtSize u => EscapedText -> DrawingInfo (AdvanceVec u)
textVector esc = let cs = getEscapedText esc in 
   foldrM (\c v -> charVector c >>= \cv -> return  (v ^+^ cv)) (vec 0 0) cs


charVector :: FromPtSize u => EscapedChar -> DrawingInfo (AdvanceVec u)
charVector (CharLiteral c) = unCF1 (ord c) avLookupTable
charVector (CharEscInt i)  = unCF1 i       avLookupTable
charVector (CharEscName s) = unCF1 ix      avLookupTable
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices


-- baseline_spacing is the vertical distance from one baseline to
-- the next it is not /gap_height/.
--

lineStartPoints :: Fractional u => u -> u -> Int -> Point2 u -> [Point2 u]
lineStartPoints cap_height baseline_spacing n (P2 x y)
    | n <  1    = []
    | n == 1    = [P2 x odd_start_y]
    | odd n     = buildList (P2 x odd_start_y) 
    | otherwise = buildList (P2 x even_start_y) 
  where
    buildList       = \pt -> take n $ iterate (.-^ vvec baseline_spacing) pt
    center_to_bl    = 0.5 * cap_height
    half_gap_height = 0.5 * (baseline_spacing - cap_height)
    halfn           = fromIntegral $ n `div` 2
    odd_start_y     = y - center_to_bl + halfn * baseline_spacing
    even_start_y    = y - half_gap_height - cap_height + halfn * baseline_spacing


annotateStartPoints :: Fractional u 
                 => u -> u -> Point2 u -> [InterimText1 u] 
                 -> [(InterimText1 u, Point2 u)]
annotateStartPoints _          _             _        []     = []
annotateStartPoints cap_height baseline_span (P2 x y) (z:rest) = 
    let y0 = if odd list_len then odd_start_y else even_start_y
        p0 = P2 x y0
    in (z,p0) : step (moveDown1 p0) rest
  where
    list_len        = 1 + length rest
    halfn           = fromIntegral $ list_len `div` 2
    center_to_bl    = 0.5 * cap_height
    half_gap_height = 0.5 * (baseline_span - cap_height)
    odd_start_y     = y - center_to_bl + halfn * baseline_span
    even_start_y    = y - half_gap_height - cap_height + halfn * baseline_span

    moveDown1       = \pt -> pt .-^ vvec baseline_span

    step _  []      = []
    step pt (a:as)  = (a,pt) : step (moveDown1 pt) as

