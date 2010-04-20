{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Charcoal.Picture
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------

module Graphics.Charcoal.Picture 
  (
  -- * Basic types
    Greyscale
  , Point(..)
  , Vector(..)
  , Picture
  , Transform

  -- * Trasformations
  , scale
  , rotate

  -- * Colours
  , black
  , white

  -- * Uniform fill
  , fill

  

  -- * Rendering
  , draw
  , drawIO

  ) where

import Data.List ( unfoldr )


type Greyscale = Double

data Point = P2 !Double !Double
  deriving (Eq,Show)

data Vector = V2 !Double !Double 
  deriving (Eq,Show)

type Picture = Point -> Greyscale 

type Transform = Point -> Point 


--------------------------------------------------------------------------------
-- Transformations

scale :: Vector -> Transform
scale (V2 sx sy) = \(P2 x y) -> P2 (x * sx) (y * sy)

rotate :: Double -> Transform
rotate ang = \(P2 x y) -> P2 (ca * x + sa * y) (ca * y + sa * x)
  where
    ca = cos ang
    sa = sin ang


--------------------------------------------------------------------------------
-- Colours

black :: Greyscale
black = 0.0

white :: Greyscale
white = 1.0


--------------------------------------------------------------------------------
-- Uniform fill

fill :: Greyscale -> Picture
fill grey = \ _pt -> grey 



--------------------------------------------------------------------------------
-- Finally, rendering...

draw :: Picture -> String
draw picf = unlines $ map (drawLine `flip` picf) [0..39]

drawIO :: Picture -> IO ()
drawIO picf = mapM_ (putStrLn . (drawLine `flip` picf)) [0..39]


drawLine :: Int -> Picture -> String
drawLine ln picf = unfoldr phi 0 
  where
    phi :: Int -> Maybe (Char,Int)
    phi x | x > 79 = Nothing
    phi x          = Just (greyscale $ picf (P2 (fromIntegral x) y), x+1 )

    y = fromIntegral ln

greyscale :: Greyscale -> Char
greyscale = fn . floor . (*16.0) where
  fn  0 = '@'
  fn  1 = '#'
  fn  2 = '$'
  fn  3 = '%'
  fn  4 = '&'
  fn  5 = '!'
  fn  6 = '+' 
  fn  7 = '-'
  fn  8 = '^'
  fn  9 = ','
  fn 10 = '.' 
  fn 11 = '`'
  fn _  = ' '