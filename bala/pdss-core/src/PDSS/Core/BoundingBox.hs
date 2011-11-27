{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.BoundingBox
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bounding Box.
--
--------------------------------------------------------------------------------


module PDSS.Core.BoundingBox
  ( 

    BoundingBox(..)
  , Boundary(..)


  -- * Boundary claculations
  , objectBBox
  , messageBBox
  , atomBBox

  , bangBBox
  , toggleBBox

  , vsliderBBox
  , hsliderBBox
  
  , vradioBBox
  , hradioBBox  

  ) where 

import PDSS.Core.InternalTypes

-- | Bounding box of an object - bang, toggle, etc. represented by
--  the lower-left and upper-right corners.
-- 
-- 
data BoundingBox = BBox 
      { ll_corner :: Point
      , ur_corner :: Point 
      }
  deriving (Eq,Ord,Show)



--------------------------------------------------------------------------------
-- Boundary class

-- | Type class extracting the bounding box of an object - 
-- Picture, Path etc.
--
class Boundary t where
  boundary :: t -> BoundingBox




--------------------------------------------------------------------------------

-- Caution - supplying bottom left might not be idiomatic...


-- Object BBox 
-- 
-- Minimum width is 3 chars (any font size).
--

objectBBox :: FontSize -> Int -> Point -> BoundingBox
objectBBox sz wc bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where 
    (w,h) = objSize sz wc

objSize :: FontSize -> Int -> (Int,Int)
objSize FONT_08 = objSize08
objSize FONT_10 = objSize10
objSize FONT_12 = objSize12
objSize FONT_16 = objSize16
objSize FONT_24 = objSize24 
objSize FONT_36 = objSize36



-- 
-- > 8 pt. Height 16 px
--
--  1 char  -  20 px
--  2 chars -  20 px
--  3 chars -  20 px
--  4 chars -  25 px
--  5 chars -  30 px 
--  6 chars -  35 px
--
--  8 chars -  45 px
--  9 chars -  50 px
-- 10 chars -  55 px

objSize08 :: Int -> (Int,Int)
objSize08 i | i < 4     = (20,16)
            | otherwise = let n = i - 3 in (20 + 5 * n, 16)



-- 
-- > 10 pt. Height 18 px
--
--  1 char  -  23 px
--  2 chars -  23 px
--  3 chars -  23 px
--  4 chars -  29 px
--  5 chars -  35 px 
--  6 chars -  41 px
--
--  8 chars -  53 px
--  9 chars -  59 px
-- 10 chars -  65 px

objSize10 :: Int -> (Int,Int)
objSize10 i | i < 4     = (23,18)
            | otherwise = let n = i - 3 in (23 + 6 * n, 18) 


-- 
-- > 12 pt. Height 21 px
--
--  1 char  -  26 px
--  2 chars -  26 px
--  3 chars -  26 px
--  4 chars -  33 px
--  5 chars -  40 px 
--  6 chars -  47 px
--
--  8 chars -  61 px
--  9 chars -  68 px
-- 10 chars -  75 px


objSize12 :: Int -> (Int,Int)
objSize12 i | i < 4     = (26,21)
            | otherwise = let n = i - 3 in (26 + 7 * n, 21) 

-- 
-- > 16 pt. Height 24 px
--
--  1 char  -  35 px
--  2 chars -  35 px
--  3 chars -  35 px
--  4 chars -  45 px
--  5 chars -  55 px 
--  6 chars -  65 px
--
--  8 chars -  85 px
--  9 chars -  95 px
-- 10 chars - 105 px


objSize16 :: Int -> (Int,Int)
objSize16 i | i < 4     = (35,24)
            | otherwise = let n = i - 3 in (35 + 10 * n, 24) 



-- 
-- > 24 pt. Height 34 px
--
--  1 char  -  47 px
--  2 chars -  47 px
--  3 chars -  47 px
--  4 chars -  61 px
--  5 chars -  75 px 
--  6 chars -  89 px
--
--  8 chars - 117 px
--  9 chars - 131 px
-- 10 chars - 145 px


objSize24 :: Int -> (Int,Int)
objSize24 i | i < 4     = (47,34)
            | otherwise = let n = i - 3 in (47 + 14 * n, 34) 



-- 
-- > 36 pt. Height 49 px
--
--  1 char  -  74 px
--  2 chars -  74 px
--  3 chars -  74 px
--  4 chars -  97 px
--  5 chars - 120 px 
--  6 chars - 143 px
--
--  8 chars - 189 px
--  9 chars - 212 px
-- 10 chars - 235 px


objSize36 :: Int -> (Int,Int)
objSize36 i | i < 4     = (74,34)
            | otherwise = let n = i - 3 in (74 + 23 * n, 34) 


-- Msg boxes.

-- 
-- All boxes are same height as object boxes.
--
--  8 pt - width  +4 px
-- 10 pt - width  +4 px
-- 12 pt - width  +5 px
-- 16 pt - width  +5 px
-- 24 pt - width  +8 px
-- 36 pt - width +10 px



messageBBox :: FontSize -> Int -> Point -> BoundingBox
messageBBox sz cc bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where 
    (w,h) = msgSize sz cc

msgSize :: FontSize -> Int -> (Int,Int)
msgSize FONT_08 = extObjWidth 4 . objSize08
msgSize FONT_10 = extObjWidth 4 . objSize10
msgSize FONT_12 = extObjWidth 5 . objSize12
msgSize FONT_16 = extObjWidth 5 . objSize16
msgSize FONT_24 = extObjWidth 8 . objSize24 
msgSize FONT_36 = extObjWidth 10 . objSize36

extObjWidth :: Int -> (Int,Int) -> (Int,Int)
extObjWidth x1 (x0,y0) = (x0 + x1,y0)


-- | Float or symbol atoms.
--
atomBBox :: FontSize -> Int -> Point -> BoundingBox
atomBBox sz cc bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where 
    (w,h) = atomSize sz cc

atomSize :: FontSize -> Int -> (Int,Int)
atomSize FONT_08 = atomSize08
atomSize FONT_10 = atomSize10
atomSize FONT_12 = atomSize12
atomSize FONT_16 = atomSize16
atomSize FONT_24 = atomSize24 
atomSize FONT_36 = atomSize36


-- 
-- > 8 pt. Height 15 px
--
--  5 chars -  28 px 
--  6 chars -  33 px
--  7 chars -  38 px
--  8 chars -  43 px
--  9 chars -  48 px
-- 10 chars -  53 px
-- 
--Iincrement 5.
--
-- No \"minimum\" size - 1 char width is less wide than 2 chars, 
-- which in turn is less than 3 chars.
--
atomSize08 :: Int -> (Int,Int)
atomSize08 i = (3 + 5 * i, 15)

-- 
-- > 10 pt. Height 17 px
--
-- 
--  5 chars -  33 px 
--  6 chars -  39 px
--  7 chars -  45 px
--  8 chars -  51 px
--  9 chars -  57 px
-- 10 chars -  63 px
-- 
-- Increment 6
--
atomSize10 :: Int -> (Int,Int)
atomSize10 i = (3 + 6 * i, 17)

-- 
-- > 12 pt. Height 20 px
--
-- 
--  5 chars -  38 px 
--  6 chars -  45 px
--  7 chars -  52 px
--  8 chars -  59 px
--  9 chars -  68 px
-- 10 chars -  73 px
-- 
-- Increment 7
--
atomSize12 :: Int -> (Int,Int)
atomSize12 i = (3 + 7 * i, 20)






-- 
-- > 16 pt. Height 23 px
--
-- 
--  5 chars -  53 px 
--  6 chars -  63 px
--  7 chars -  73 px
--  8 chars -  83 px
--  9 chars -  93 px
-- 10 chars - 103 px
-- 
-- Increment 10
--
atomSize16 :: Int -> (Int,Int)
atomSize16 i = (3 + 10 * i, 23)

-- 
-- > 24 pt. Height 33 px
--
-- 
--  5 chars -  73 px 
--  6 chars -  87 px
--  7 chars - 101 px
--  8 chars - 115 px
--  9 chars - 129 px
-- 10 chars - 143 px
-- 
-- Increment 14
--
atomSize24 :: Int -> (Int,Int)
atomSize24 i = (3 + 14 * i, 33)


-- 
-- > 36 pt. Height 48 px
--
-- 
--  5 chars - 118 px 
--  6 chars - 141 px
--  7 chars - 164 px
--  8 chars - 187 px
--  9 chars - 210 px
-- 10 chars - 233 px
-- 
-- Increment 23
--
atomSize36 :: Int -> (Int,Int)
atomSize36 i = (3 + 23 * i, 48)


-- | TODO - bang and toggle are actually configurable by the 
-- size param.
-- 
-- By the looks of things width and height are 1+ the size.
--
bangBBox :: Point -> BoundingBox 
bangBBox bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where
    w = 16
    h = 16



toggleBBox :: Point -> BoundingBox
toggleBBox bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where
    w = 16
    h = 16



-- Slider

-- The default vslider is drawn with 16px width x 134px height.
-- This is recorded in the Pd file as 15,128
--
-- The 20,150 slider is drawn with 21px width x 156px height.
-- 
-- So it looks like the algorithm is to add 1px to the width and
-- 6px to the height.
--

vsliderBBox :: Int -> Int -> Point -> BoundingBox
vsliderBBox w0 h0 bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where
    w = w0 + 1
    h = h0 + 6


hsliderBBox :: Int -> Int -> Point -> BoundingBox
hsliderBBox w0 h0 bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where
    w = w0 + 6
    h = h0 + 1



-- Radio is oblivious to font size.


-- > Width 16 px
--
--  2 choices -  31 px
--  3 choices -  46 px
--  4 choices -  61 px
--  5 choices -  76 px
--  6 choices -  91 px 
--  7 choices - 106 px
--  8 choices - 121 px

vradioBBox :: Int -> Point -> BoundingBox
vradioBBox i bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where
    w = 16
    h = let n = i - 1 in 16 + 15 * n


hradioBBox :: Int -> Point -> BoundingBox
hradioBBox i bl@(P2 x y) = BBox bl (P2 (x+w) (y+h))
  where
    h = 16
    w = let n = i - 1 in 16 + 15 * n
