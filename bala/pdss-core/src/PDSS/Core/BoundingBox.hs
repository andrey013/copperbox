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

  , objectBBox

  , bangBBox
  , toggleBBox

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
    (w,h) = boundSize sz wc

boundSize :: FontSize -> Int -> (Int,Int)
boundSize FONT_08 = boundSize08
boundSize FONT_10 = boundSize10
boundSize FONT_12 = boundSize12
boundSize FONT_16 = boundSize16
boundSize FONT_24 = boundSize24 
boundSize FONT_36 = boundSize36



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

boundSize08 :: Int -> (Int,Int)
boundSize08 i | i < 4     = (20,16)
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

boundSize10 :: Int -> (Int,Int)
boundSize10 i | i < 4     = (23,18)
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


boundSize12 :: Int -> (Int,Int)
boundSize12 i | i < 4     = (26,21)
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


boundSize16 :: Int -> (Int,Int)
boundSize16 i | i < 4     = (35,24)
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


boundSize24 :: Int -> (Int,Int)
boundSize24 i | i < 4     = (47,34)
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


boundSize36 :: Int -> (Int,Int)
boundSize36 i | i < 4     = (74,34)
              | otherwise = let n = i - 3 in (74 + 23 * n, 34) 


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
