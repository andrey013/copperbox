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

module Wumpus.Clave.DjembeStrokes
  (
  
    bass
  , muffledBass
  , slap
  , tone
  , muffledTone

  , dot

  , paren
  , dominant
  , otherhand

  ) where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )     
import Wumpus.Basic.Graphic             -- package: wumpus-basic
import Wumpus.Basic.SafeFonts 

import Data.AffineSpace                 -- package: vector-space


bass            :: DGraphicF
bass            = letter 'B'

muffledBass     :: DGraphicF
muffledBass     = slash `cc` bass

tone            :: DGraphicF
tone            = circle black 4

muffledTone     :: DGraphicF
muffledTone     = slash `cc` tone

slap            :: DGraphicF
slap            = letter 'X'

dot             :: DGraphicF
dot             = circle black 0.5 . moveOrigin 0 3

paren :: DGraphicF -> DGraphicF
paren x = lparen `cc` x `cc` rparen
  where
    lparen = letter '(' . moveOrigin 3    (-1)
    rparen = letter ')' . moveOrigin (-7) (-1)

dominant :: DGraphicF -> DGraphicF
dominant = (closed_square `cc`)
  where
    closed_square = filledRectangle black  4.5 4.5 . moveOrigin 2.25 9.25

otherhand :: DGraphicF -> DGraphicF
otherhand = (open_square `cc`)
  where
    open_square = strokedRectangle props 4 4 . moveOrigin 2 9
    props       = (black, LineWidth 0.5)

-- Note - we cannot make a "rectangle transformer" that centers a 
-- rectangle because the Bounding box cannot be accessed.
--
-- Instead we would have to create rectangles and transform
-- the origin while the width & height are in scope
-- 


--------------------------------------------------------------------------------

-- Where to have the  origin....
letter :: Char -> DGraphicF
letter ch = grp . moveOrigin 4 5
  where 
    grp = wrapG . textlabel (black,helvetica12) [ch]

slash :: DGraphicF
slash = straightLine black (V2 8 14) . moveOrigin 4 7



-- Probably for Basic.Graphic

type Point2F u = Point2 u -> Point2 u

moveOrigin :: Num u => u -> u -> Point2F u
moveOrigin x y = (.-^ V2 x y)


-- Wow a new bird combinator...

infixr 9 `cc`

cc :: (r1 -> a -> ans) -> (r1 -> r2 -> a) -> r1 -> r2 -> ans
cc f g = \x y -> f x (g x y)
