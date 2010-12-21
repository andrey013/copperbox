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

import Control.Applicative
import Data.Char
import Data.Foldable ( foldrM )
import qualified Data.Map               as Map
import Data.Maybe 


-- Note - BoundedLocThetaGraphic is probably an adequate type
-- even though the same text will have a different bounding box
-- if it is rotated (the sides of the BBox are always parallel to 
-- the x and y axes even if the text is not parrale to the 
-- x-axis). 
-- 
-- I cannot think of any compelling graphics that need a more 
-- accurate type. The execption is text cannot have exact anchors 
-- however this is a moot /if/ text is considered as a labelling 
-- of an existing rectangle (which may or may not have been 
-- rotated).
--


-- Code below is out-of-date. Text should be a 
-- BoundedLocThetaGraphic not a BoundedLocGraphic.



-- One line of multiline text
--
data InterimText1 u = InterimText1
      { text1_escaped   :: EscapedText
      , text1_advance   :: AdvanceVec u
      } 
  deriving (Eq,Show)


-- Draw one line of left-aligned text, knowing the max_width of
-- all the lines of text.
--
-- All left-aligned text is moved left by half the max_width.
--
-- Impilict point is baseline-center.
--
drawLeftAligned :: Floating u => u -> InterimText1 u -> LocThetaGraphic u
drawLeftAligned max_width (InterimText1 esc _) = 
    promoteR2 $ \baseline_ctr theta -> 
       let mv = displaceParallel (negate $ 0.5* max_width) theta
       in apply2R2 (rescapedline esc) (mv baseline_ctr) theta
       


-- Draw one line of center-aligned text. Center aligned text is 
-- oblivious to the max_width of all the lines of text.
--
-- Each line of center-aligned text is moved left by half its 
-- advance vector.
--
-- Impilict point is baseline-center.
--
drawCenterAligned :: Floating u => u -> InterimText1 u -> LocThetaGraphic u
drawCenterAligned _ (InterimText1 esc av) = 
    promoteR2 $ \baseline_ctr theta -> 
       let mv = displaceParallel (negate $ 0.5 * advanceH av) theta
       in apply2R2 (rescapedline esc) (mv baseline_ctr) theta
       

-- Draw one line of right-aligned text, knowing the max_width of
-- all the lines of text.
--
-- Each right-aligned text line is moved by the width component 
-- of the advance vector minus half the max width.
--
-- Impilict point is baseline-center.
--
drawRightAligned :: Floating u => u -> InterimText1 u -> LocThetaGraphic u
drawRightAligned max_width (InterimText1 esc av) = 
    promoteR2 $ \baseline_ctr theta -> 
       let mv = displaceParallel (advanceH av - (0.5 * max_width)) theta
       in apply2R2 (rescapedline esc) (mv baseline_ctr) theta










-- | Implicit origin of the text is baseline-left.
--
singleLineBL :: (Ord u, FromPtSize u) 
             => String -> BoundedLocGraphic u
singleLineBL ss = lift0R1 (interimText1 ss) >>= singleLRText id 

-- | Implicit origin of the text is center-center.
--
singleLineCC :: (Fractional u, Ord u, FromPtSize u) 
             => String -> BoundedLocGraphic u
singleLineCC ss = lift0R1 glyphCapHeight  >>= \cap_h   -> 
                  lift0R1 (interimText1 ss) >>= \interim -> 
                  let hw = 0.5 * advanceH (text1_advance interim)
                  in singleLRText (.-^ vec hw (0.5 * cap_h)) interim


-- | Implicit origin of the text is baseline-center.
--
singleLineBC :: (Fractional u, Ord u, FromPtSize u) 
             => String -> BoundedLocGraphic u
singleLineBC ss = lift0R1 (interimText1 ss) >>= \interim -> 
                  let hw = 0.5 * advanceH (text1_advance interim)
                  in singleLRText (.-^ hvec hw) interim



escCharBC :: (Fractional u, Ord u, FromPtSize u)  
          => EscapedChar -> BoundedLocGraphic u
escCharBC ch = let esc = wrapEscChar ch in 
    lift0R1 (textVector esc) >>= \v ->
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
    promoteR1 $ \p0 -> 
      linesToInterims ss >>= \(wv,xs)   ->
      glyphCapHeight     >>= \cap_h     -> 
      baselineSpacing    >>= \base_span ->   
      let p1  = dispF wv p0
          axs = annotateStartPoints cap_h base_span p1 xs
      in mergeLines drawF p1 axs

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
             => PointDisplace u -> InterimText1 u -> BoundedLocGraphic u
singleLRText dispF (InterimText1 esc av) =
    promoteR1 $ \p0 -> let pt = dispF p0 in 
      apply1R1 (realTextBounds av) pt >>= \bb ->
        fmap (replaceL bb) $ escapedline esc `at` pt


-- Note - ymin is (usually) negative...
--
realTextBounds :: (Ord u, FromPtSize u) 
               => AdvanceVec u -> LocDrawingInfo u (BoundingBox u)
realTextBounds av = 
    promoteR1 $ \pt -> 
      glyphHeightRange >>= \(ymin, ymax)  ->
        let w   = advanceH av
            ll  = pt .+^ vvec ymin
            ur  = pt .+^ vec w ymax
        in pure $ boundingBox ll ur

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
    fmap (mk esc) $ textVector esc
  where
    mk a b = InterimText1 { text1_escaped = a
                          , text1_advance = b }


textVector :: FromPtSize u => EscapedText -> DrawingInfo (AdvanceVec u)
textVector esc = let cs = destrEscapedText id esc in 
   foldrM (\c v -> charVector c >>= \cv -> return  (v ^+^ cv)) (vec 0 0) cs


charVector :: FromPtSize u => EscapedChar -> DrawingInfo (AdvanceVec u)
charVector (CharLiteral c) = (\fn -> fn $ ord c) <$> avLookupTable
charVector (CharEscInt i)  = (\fn -> fn i)       <$> avLookupTable
charVector (CharEscName s) = (\fn -> fn ix)      <$> avLookupTable
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices



-- baseline_span is the vertical distance from one baseline to
-- the next it is not /gap_height/.
--

-- Yikes!

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

