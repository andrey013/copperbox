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


muffledBass     :: DGraphicF
muffledBass     = slash bassP

muffledTone     :: DGraphicF
muffledTone     = slash toneP

dot             :: DGraphicF
dot             = circle black 0.5 . moveOrigin 0 3

bass            :: DGraphicF
bass            = wrapG . bassP

tone            :: DGraphicF
tone            = wrapPrimF toneP

slap            :: DGraphicF
slap            = wrapPrimF slapP

bassP           :: DPrimF
bassP           = letter 'B'

toneP           :: DPrimF
toneP           = ellipse black 4 4

slapP           :: DPrimF
slapP           = letter 'X'


slash :: DPrimF -> DGraphicF
slash primF = slash1 `cc` (wrapG . primF)
  where
    slash1 = straightLine black (V2 8 14) . moveOrigin 4 7



paren :: DPrimF -> DGraphicF
paren primF = lparen `cc` (wrapG . primF) `cc` rparen
  where
    lparen = wrapG . letter '(' . moveOrigin 3    (-1)
    rparen = wrapG . letter ')' . moveOrigin (-7) (-1)

dominant :: DGraphicF -> DGraphicF
dominant grF = closed_square `cc` grF
  where
    closed_square = filledRectangle black  4.5 4.5 . moveOrigin 2.25 9.25

otherhand :: DGraphicF -> DGraphicF
otherhand grF = open_square `cc` grF
  where
    open_square = strokedRectangle props 4 4 . moveOrigin 2 9
    props       = (black, LineWidth 0.5)

-- Note - we cannot make a "rectangle transformer" that centers a 
-- rectangle because the Bounding box cannot be accessed.
--
-- Instead we would have to create rectangles and transform
-- the origin while the width & height are in scope
-- 

-- flams are need not necessarily duplicate the same letter
-- GDgdPT notation can have [GD,dg,PT,TP,...]


-- sub is an 80% scale - sub bottom-right is sup top-left
flam :: DPrimF -> DPrimF -> DGraphicF
flam sub sup = (wrapG . sup) `cc` (wrapG . trafo sub) 
  where
    trafo f = \pt -> translate (-4) 6 $ uniformScale 0.6 $ f pt

    -- this is wrong - scalings (like the more obvious rotate) need to 
    -- be done about the origin...


--
-- Notes - in a nutshell you can't affine transform GraphicF.
--
-- With a Hughes list you can do some silly things, e.g
--
-- > list1 = map (+1) . fromListH [1,2,3] 
-- 
-- But the extent of map (+1) will travel to anything that
-- list1 is concatenated to.
--
-- > list2 = list1 . fromList [0,0,0]
--
-- > list2 == [2,3,4,1,1,1]
--
-- In theory you a trasfomation could go in-and-out of
-- an normal list
--
-- > fromListH . map (scale x y) . toListH
--
-- But this is beyond the pale...
--


--------------------------------------------------------------------------------

type PrimF  u = Point2 u -> Primitive u
type DPrimF   = PrimF Double



wrapPrimF :: (Point2 u -> Primitive u) -> GraphicF u
wrapPrimF fn = wrapG . fn


-- Where to have the  origin....
letter :: Char -> DPrimF
letter ch = grp . moveOrigin 4 5
  where 
    grp = textlabel (black,helvetica12) [ch]


-- Probably for Basic.Graphic

type Point2T    u = Point2 u    -> Point2 u
type PrimitiveT u = Primitive u -> Primitive u 


moveOrigin :: Num u => u -> u -> Point2T u
moveOrigin x y = (.-^ V2 x y)


-- Wow a new bird combinator...

infixr 9 `cc`

cc :: (r1 -> a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
cc f g = \x y -> f x (g x y)
