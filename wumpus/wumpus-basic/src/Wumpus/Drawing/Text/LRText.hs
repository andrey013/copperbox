{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.LRText
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

module Wumpus.Drawing.Text.LRText
  ( 

   
    singleLineBL
  , singleLineBC
  , singleLineCC
  , escCharBC

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight


  ) where


import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Char
import Data.Foldable ( foldrM )
import qualified Data.Map               as Map
import Data.Maybe 


-- One line of multiline text
--
data InterimText1 u = InterimText1
      { text1_escaped   :: EscapedText
      , text1_advance   :: (AdvanceVec u)
      } 
  deriving (Eq,Show)

-- | Implicit origin of the text is baseline-left.
--
singleLineBL :: (Ord u, FromPtSize u) 
             => String -> BoundedLocGraphic u
singleLineBL ss = interimText1 ss >>= singleLRText id 

-- | Implicit origin of the text is center-center.
--
singleLineCC :: (Fractional u, Ord u, FromPtSize u) 
             => String -> BoundedLocGraphic u
singleLineCC ss = glyphCapHeight  >>= \cap_h   -> 
                  interimText1 ss >>= \interim -> 
                  let hw = 0.5 * advanceH (text1_advance interim)
                  in singleLRText (.-^ vec hw (0.5 * cap_h)) interim


-- | Implicit origin of the text is baseline-center.
--
singleLineBC :: (Fractional u, Ord u, FromPtSize u) 
             => String -> BoundedLocGraphic u
singleLineBC ss = interimText1 ss >>= \interim -> 
                  let hw = 0.5 * advanceH (text1_advance interim)
                  in singleLRText (.-^ hvec hw) interim



escCharBC :: (Fractional u, Ord u, FromPtSize u)  
          => EscapedChar -> BoundedLocGraphic u
escCharBC ch = let esc = wrapEscChar ch in 
    textVector esc >>= \v ->
    let hw = 0.5 * advanceH v
    in singleLRText (.-^ hvec hw) (InterimText1 { text1_escaped = esc
                                                , text1_advance = v })



-- | Draw multi-line text, aligned to the left. 
--
-- The input string is split on newline with the Prelude function 
-- @lines@. The supplied point is the center of the text.
--
multiAlignLeft      :: (Fractional u, Ord u, FromPtSize u) 
                    => String -> BoundedLocGraphic u
multiAlignLeft      = 
    multiAligned drawLeftAligned1 (\wv pt -> pt .-^ hvec (0.5 * advanceH wv))


-- | Draw multi-line text, aligned on the horizontal center. 
--
-- The input string is split on newline with the Prelude function 
-- @lines@. The supplied point is the center of the text.
--
multiAlignCenter    :: (Fractional u, Ord u, FromPtSize u) 
                    => String -> BoundedLocGraphic u
multiAlignCenter    = 
    multiAligned drawCenterAligned1 (\_ pt -> pt)

-- | Draw multi-line text, aligned to the right. 
--
-- The input string is split on newline with the Prelude function 
-- @lines@. The supplied point is the center of the text.
--
multiAlignRight     :: (Fractional u, Ord u, FromPtSize u) 
                    => String -> BoundedLocGraphic u
multiAlignRight     = 
    multiAligned drawRightAligned1 (\wv pt -> pt .+^ hvec (0.5 * advanceH wv))


-- Build multi-line aligned text. 
--
-- The drawF functions regard the supplied point differently, for
-- instance @drawRightAligned1@ regards the point as 
-- baseline-right.
--
-- The dispF are applied to a point which is initially in the 
-- center of the drawing - for right aligned text it is displaced 
-- to the right (half the max width vectoer), for left aligned 
-- text it is displaced to the left. Center aligned text is not 
-- displaced.
--
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




singleLRText :: (Ord u, FromPtSize u) 
             => (Point2 u -> Point2 u) -> InterimText1 u 
             -> BoundedLocGraphic u
singleLRText dispF (InterimText1 esc av) =
    glyphHeightRange >>= \(ymin, ymax)  ->
    promote1 $ \p0 -> let pt  = dispF p0
                          w   = advanceH av
                          ll  = pt .+^ vvec ymin
                          ur  = pt .+^ vec w ymax
                          bb  = boundingBox ll ur 
                      in (escapedline esc `at` pt) >>= \prim ->
                         return (bb, prim)



-- Point is baseline-left
--
drawLeftAligned1 :: (Ord u, FromPtSize u) 
                 => InterimText1 u -> Point2 u -> BoundedGraphic u
drawLeftAligned1 itext pt = singleLRText id itext `at` pt

-- Point is baseline-center
--
drawCenterAligned1 :: (Fractional u, Ord u, FromPtSize u)
                   => InterimText1 u -> Point2 u -> BoundedGraphic u
drawCenterAligned1 itext pt = 
    let hw = 0.5 * advanceH (text1_advance itext)    
    in singleLRText (.-^ hvec hw) itext `at` pt 

-- Point is baseline-right
--
drawRightAligned1 :: (Fractional u, Ord u, FromPtSize u)
                  => InterimText1 u -> Point2 u -> BoundedGraphic u
drawRightAligned1 itext pt = 
    let w = advanceH (text1_advance itext)
    in singleLRText (.-^ hvec w) itext `at` pt




-- This isn't worth the complexity  to get to one traversal...

linesToInterims :: (FromPtSize u, Ord u) 
                => String -> DrawingInfo (AdvanceVec u, [InterimText1 u])
linesToInterims = fmap post . mapM interimText1 . lines
  where
    post xs   = let vmax = foldr fn (hvec 0) xs in (vmax,xs)
    fn a vmax = avMaxWidth (text1_advance a) vmax

avMaxWidth :: Ord u => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
avMaxWidth a@(V2 w1 _) b@(V2 w2 _) = if w2 > w1 then b else a

interimText1 :: FromPtSize u => String -> DrawingInfo (InterimText1 u)
interimText1 ss = let esc = escapeString ss in 
    postpro (mk esc) $ textVector esc
  where
    mk a b = InterimText1 { text1_escaped = a
                          , text1_advance = b }


textVector :: FromPtSize u => EscapedText -> DrawingInfo (AdvanceVec u)
textVector esc = let cs = destrEscapedText id esc in 
   foldrM (\c v -> charVector c >>= \cv -> return  (v ^+^ cv)) (vec 0 0) cs


charVector :: FromPtSize u => EscapedChar -> DrawingInfo (AdvanceVec u)
charVector (CharLiteral c) = unCF1 (ord c) avLookupTable
charVector (CharEscInt i)  = unCF1 i       avLookupTable
charVector (CharEscName s) = unCF1 ix      avLookupTable
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices



-- baseline_span is the vertical distance from one baseline to
-- the next it is not /gap_height/.
--

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

