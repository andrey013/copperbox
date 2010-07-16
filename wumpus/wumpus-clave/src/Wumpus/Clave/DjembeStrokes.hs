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
stemC gF = stem `cc` (gF . vdisplace (-25))

flamC :: DGraphicF -> DGraphicF -> DGraphicF
flamC sF lF = flamStem `cc` (sF . displace (-5) (-18)) 
                       `cc` (lF . vdisplace (-25))


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
    smallTone = circle black 2 . vdisplace (-1)



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
toneP           = circle black 4 . vdisplace (-1)

slapP           :: DGraphicF
slapP           = letter 'X'

dotP            :: DGraphicF
dotP            = circle black 1 . vdisplace (-3)



paren :: DGraphicF -> DGraphicF
paren gF = lparen `cc` gF `cc` rparen
  where
    lparen = letter '(' . displace (-4) (-24)
    rparen = letter ')' . displace 8    (-24)


slash :: DGraphicF -> DGraphicF
slash gF = gF `cc` slash1
  where
    slash1 = straightLine black (V2 10 10) . displace (-5) (-6) 


underscore :: DGraphicF -> DGraphicF
underscore gF = gF `cc` uscore
  where
    uscore = straightLine black (V2 10 0) . displace (-5) (-5) 


dominant :: DGraphicF -> DGraphicF
dominant gF = gF `cc` closed_square
  where
    closed_square = filledRectangleCtr black  4.5 4.5 . displaceHand

otherhand :: DGraphicF -> DGraphicF
otherhand = (`cc` open_square)
  where
    open_square = strokedRectangleCtr props 4 4 . displaceHand
    props       = (black, LineWidth 0.5)

displaceHand :: Point2T Double
displaceHand = vdisplace (-3) . displaceStem . displaceCharHeight 

accent :: DGraphicF -> DGraphicF
accent = (`cc` gt)
  where  
    gt = openPath line_props [ vec 10 3, vec (-10) 3] . displace (-5) 2


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
displaceStem = vdisplace (-20)

displaceCharHeight :: Point2T Double
displaceCharHeight = vdisplace (-10)


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
letter ch = wrapG . textlabel (black,helvetica 12) [ch] . displace (-4) (-5)

-- For flam...
smallLetter :: Char -> DGraphicF
smallLetter ch = wrapG . textlabel (black,helvetica 9) [ch] . displace (-3) (-5)


-- Candidates for Basic.Graphic

type Point2T    u = Point2 u -> Point2 u


displace :: Num u => u -> u -> Point2T u
displace x y = (.+^ V2 x y)

hdisplace :: Num u => u -> Point2T u
hdisplace x = displace x 0

vdisplace :: Num u => u -> Point2T u
vdisplace y = displace 0 y




openPath :: (Stroke t, Num u) => t -> [Vec2 u] -> GraphicF u
openPath t vs = \pt -> wrapG $ ostroke t $ path pt (snd $ mapAccumL fn pt vs)
  where
    fn p v = let p' = p .+^ v in (p', lineTo p')




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
