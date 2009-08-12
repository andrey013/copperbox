{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Label
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Text labels 
-- (and .eps at some point?)
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Label where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Instances ()
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Data.AffineSpace
import Data.VectorSpace

-- Labels are drawn inside a clipping rectangle. This puts a burden on the 
-- user to check the output to see that the clipping rect is large enough
-- for the text it contains.
-- 
-- One option would be to add support for em/ex for some of the standard
-- PostSript fonts (Helvetica, Times-Roman). em is the approx width of 
-- 'M' in the current font, ex is the approx height of 'x'. 
-- It would not be too burdensome to measure a few fonts...

data Label pt = Label { 
      labelText       :: String,
      labelBottomLeft :: pt,
      labelRect       :: BoundingBox pt,
      labelLineHeight :: Scalar (Diff pt) 
    }



-- Note - Labels use a clipping path so they must be bracketed with  
-- saveExecRestore. 

label :: String -> Double -> Double -> DPoint2 -> Label (Point2 Double)
label ss w h pt = Label ss pt bbox h where
    bbox = BBox pt (pt .+^ (V2 w h))

text :: String -> Double -> Double -> Double -> DPoint2 -> Label (Point2 Double)
text ss w h lh pt = Label ss pt bbox lh where
    bbox = BBox pt (pt .+^ (V2 w h))



displaceLabel :: (Num a, AffineSpace pt, Diff pt ~ v, Scalar v ~ a) 
              => v -> Label pt -> Label pt
displaceLabel v (Label s p (BBox a b) lh) = 
  Label s (p .+^ v) (BBox (a .+^ v) (b .+^ v)) lh



{-
withFrame $ \frm -> (mf frm, getBoundingBox $ cliprect frm)
  where
    mf frm = saveExecRestore id $ clipPolygon (cliprect frm) 
                                $ drawLabel text (origin frm)
    cliprect frm = (rectangle w h) (origin frm)
-}

{-

-- multi-line text is generally achieved in PostScript with moveto
-- acting as a carriage return.

-- currently this draws from bottom to top (hence the reverse of the list),
-- it needs improving...
picText :: String -> Double -> Double -> Double -> Picture
picText text w h line_height = withFrame $ \frm -> (mf frm, getBoundingBox $ cliprect frm)
  where
    
    mf frm = saveExecRestore id $ clipPolygon (cliprect frm) 
                                $ zipWithM_ drawLabel (reverse ls) (points $ origin frm)
    cliprect frm = (rectangle w h) (origin frm)
    
    ls = lines text
    points pt = take (length ls) $ iterate (.+^ (V2 0 (line_height))) pt

    
-}



