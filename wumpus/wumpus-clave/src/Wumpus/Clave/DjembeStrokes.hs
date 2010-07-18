{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Clave.DjembeStrokes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe
--
--------------------------------------------------------------------------------

module Wumpus.Clave.DjembeStrokes where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )     
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts 

import Data.AffineSpace                 -- package: vector-space

import Data.List


-- need a combinator to draw a stem and "feed" a 
-- center coord to the argument...

stemC :: DGraphicF -> DGraphicF
stemC gF = stem `cc` (gF . vdisp (-25))

flamC :: DGraphicF -> DGraphicF -> DGraphicF
flamC sF lF = flamStem `cc` (sF . disp (-5) (-18)) 
                       `cc` (lF . vdisp (-25))


-- Top of the stem is the origin...

muffledBass     :: DGraphicF
muffledBass     = stemC (slash bassP)

muffledTone     :: DGraphicF
muffledTone     = stemC (slash toneP)

muffledSlap     :: DGraphicF
muffledSlap     = stemC (underscore slapP)

bassFlam        :: DGraphicF
bassFlam        = flamC (smallLetter 'B') (letter 'B')


slapFlam        :: DGraphicF
slapFlam        = flamC (smallLetter 'X') (letter 'X')

toneFlam        :: DGraphicF
toneFlam        = flamC smallTone toneP
  where
    smallTone = disk black 2 . vdisp (-1)



dot             :: DGraphicF
dot             = stemC dotP

bass            :: DGraphicF
bass            = stemC bassP

tone            :: DGraphicF
tone            = stemC toneP

slap            :: DGraphicF
slap            = stemC slapP

bassP           :: DGraphicF
bassP           = letter 'B'

toneP           :: DGraphicF
toneP           = disk black 4 . vdisp (-1)

slapP           :: DGraphicF
slapP           = letter 'X'

dotP            :: DGraphicF
dotP            = disk black 1 . vdisp (-3)



paren :: DGraphicF -> DGraphicF
paren gF = lparen `cc` gF `cc` rparen
  where
    lparen = letter '(' . disp (-4) (-24)
    rparen = letter ')' . disp 8    (-24)


slash :: DGraphicF -> DGraphicF
slash gF = gF `cc` slash1
  where
    slash1 = straightLine black (V2 10 10) . disp (-5) (-6) 


underscore :: DGraphicF -> DGraphicF
underscore gF = gF `cc` uscore
  where
    uscore = straightLine black (V2 10 0) . disp (-5) (-5) 


dominant :: DGraphicF -> DGraphicF
dominant gF = gF `cc` closed_square
  where
    closed_square = filledRectangle black  4.5 4.5 . displaceHand

otherhand :: DGraphicF -> DGraphicF
otherhand = (`cc` open_square)
  where
    open_square = strokedRectangle props 4 4 . displaceHand
    props       = (black, LineWidth 0.5)

displaceHand :: Point2T Double
displaceHand = vdisp (-3) . displaceStem . displaceCharHeight 

accent :: DGraphicF -> DGraphicF
accent = (`cc` gt)
  where  
    gt = openPath line_props [ vec 10 3, vec (-10) 3] . disp (-5) 2


-- 

stem :: DGraphicF
stem = straightLine line_props (vvec 20) . displaceStem

flamStem :: DGraphicF 
flamStem = openPath line_props vec_path . displaceStem
  where
    vec_path = [vvec 20, vec (-5) (-5), vvec (-10)]

line_props :: (DRGB, StrokeAttr)
line_props = (black, LineWidth 1.0)


displaceStem :: Point2T Double
displaceStem = vdisp (-20)

displaceCharHeight :: Point2T Double
displaceCharHeight = vdisp (-10)


-- Note - we cannot make a "rectangle transformer" that centers a 
-- rectangle because the Bounding box cannot be accessed.
--
-- Instead we would have to create rectangles and transform
-- the origin while the width & height are in scope
-- 

-- flams are need not necessarily duplicate the same letter
-- GDgdPT notation can have [GD,dg,PT,TP,...]
--
-- tone flam (circle) and the letter flams have different
-- scaling factors.



--------------------------------------------------------------------------------




-- Where to have the  origin....
letter :: Char -> DGraphicF
letter ch = wrapG . textlabel (black,helvetica 12) [ch] . disp (-4) (-5)

-- For flam...
smallLetter :: Char -> DGraphicF
smallLetter ch = wrapG . textlabel (black,helvetica 9) [ch] . disp (-3) (-5)


-- Candidates for Basic.Graphic




openPath :: (Stroke t, Num u) => t -> [Vec2 u] -> GraphicF u
openPath t vs = \pt -> wrapG $ ostroke t $ path pt (snd $ mapAccumL fn pt vs)
  where
    fn p v = let p' = p .+^ v in (p', lineTo p')



{-
-- | Point is center.
--
strokedRectangleCtr :: (Stroke t, Fractional u) => t -> u -> u -> GraphicF u
strokedRectangleCtr t w h = wrapG . cstroke t . rectangleCtr w h

-- | Point is center.
--
filledRectangleCtr :: (Fill t, Fractional u) => t -> u -> u -> GraphicF u
filledRectangleCtr t w h = wrapG . fill t . rectangleCtr w h

rectangleCtr :: Fractional u => u -> u -> Point2 u -> Path u
rectangleCtr w h ctr = path bl [ lineTo br, lineTo tr, lineTo tl ]
  where
    bl = ctr .-^ V2 (w*0.5) (h*0.5)
    br = bl .+^ hvec h
    tr = br .+^ vvec h
    tl = bl .+^ vvec h 
-}