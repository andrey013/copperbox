{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Djembe.Strokes
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

module Wumpus.Djembe.Strokes where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )     
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts 

import Data.AffineSpace                 -- package: vector-space

import Data.List


{-
-- need a combinator to draw a stem and "feed" a 
-- center coord to the argument...

stemC :: DLocGraphic -> DLocGraphic
stemC gF = stem `cc` (gF . vdisp (-25))

flamC :: DLocGraphic -> DLocGraphic -> DLocGraphic
flamC sF lF = flamStem `cc` (sF . disp (-5) (-18)) 
                       `cc` (lF . vdisp (-25))


-- Top of the stem is the origin...

muffledBass     :: DLocGraphic
muffledBass     = stemC (slash bassP)

muffledTone     :: DLocGraphic
muffledTone     = stemC (slash toneP)

muffledSlap     :: DLocGraphic
muffledSlap     = stemC (underscore slapP)

bassFlam        :: DLocGraphic
bassFlam        = flamC (smallLetter 'B') (letter 'B')


slapFlam        :: DLocGraphic
slapFlam        = flamC (smallLetter 'X') (letter 'X')

toneFlam        :: DLocGraphic
toneFlam        = flamC smallTone toneP
  where
    smallTone = disk black 2 . vdisp (-1)



dot             :: DLocGraphic
dot             = stemC dotP

bass            :: DLocGraphic
bass            = stemC bassP

tone            :: DLocGraphic
tone            = stemC toneP

slap            :: DLocGraphic
slap            = stemC slapP

bassP           :: DLocGraphic
bassP           = letter 'B'

toneP           :: DLocGraphic
toneP           = disk black 4 . vdisp (-1)

slapP           :: DLocGraphic
slapP           = letter 'X'

dotP            :: DLocGraphic
dotP            = disk black 1 . vdisp (-3)



paren :: DLocGraphic -> DLocGraphic
paren gF = lparen `cc` gF `cc` rparen
  where
    lparen = letter '(' . disp (-4) (-24)
    rparen = letter ')' . disp 8    (-24)


slash :: DLocGraphic -> DLocGraphic
slash gF = gF `cc` slash1
  where
    slash1 = straightLine black (V2 10 10) . disp (-5) (-6) 


underscore :: DLocGraphic -> DLocGraphic
underscore gF = gF `cc` uscore
  where
    uscore = straightLine black (V2 10 0) . disp (-5) (-5) 


dominant :: DLocGraphic -> DLocGraphic
dominant gF = gF `cc` closed_square
  where
    closed_square = filledRectangle black  4.5 4.5 . displaceHand
-}
otherhand :: DLocGraphic -> DLocGraphic
otherhand = (`oplus` open_square)
  where
    open_square = localize props $ prepro1 displaceHand $ strokedRectangle 4 4
    props       = strokeColour black . thin -- setLineWidth 0.5


displaceHand :: PointDisplace Double
displaceHand = vdisplace (-3) . displaceStem . displaceCharHeight 

accent :: DLocGraphic -> DLocGraphic
accent = (`oplus` gt)
  where  
    gt = prepro1 (displace (-5) 2) $ openPath [ vec 10 3, vec (-10) 3]


-- 

stem :: DLocGraphic
stem = prepro1 displaceStem $ straightLine (vvec 20)

flamStem :: DLocGraphic 
flamStem = prepro1 displaceStem $ openPath vec_path
  where
    vec_path = [vvec 20, vec (-5) (-5), vvec (-10)]

-- line_props :: (RGBi, StrokeAttr)
-- line_props = (black, LineWidth 1.0)


displaceStem :: PointDisplace Double
displaceStem = vdisplace (-20)

displaceCharHeight :: PointDisplace Double
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
--
letter :: Char -> DLocGraphic
letter ch = localize (fontSize 12) $ 
    prepro1 (displace (-4) (-5)) $ textline [ch]



-- For flam...
--
smallLetter :: Char -> DLocGraphic
smallLetter ch = localize (fontSize 9) $ 
   prepro1 (displace (-3) (-5)) (textline [ch])


-- Candidates for Basic.Graphic




openPath :: Num u => [Vec2 u] -> LocGraphic u
openPath vs = 
    promote1 $ \pt -> openStroke $ primPath pt (snd $ mapAccumL fn pt vs)
  where
    fn p v = let p' = p .+^ v in (p', lineTo p')


