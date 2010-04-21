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
draw picf = unlines $ map (\cy -> drawLine cy ixs picf) ixs
  where
    ixs = coords 40 

drawIO :: Picture -> IO ()
drawIO picf =  mapM_ (\cy -> putStrLn $ drawLine cy ixs picf) ixs
  where
    ixs = coords 40 

drawLine :: Double -> [Double] -> Picture -> String
drawLine cy ixs picf = foldr fn "" ixs
  where
    fn cx ss = greyscale (picf (P2 cx cy)) : ss

coords :: Int -> [Double]
coords steps = take steps $ iterate (+x) (-1.0)
  where
    x = 2.0 / fromIntegral (steps-1)


greyscale :: Greyscale -> Char
greyscale = fn . floor . (*16.0) 
  where
    fn :: Int -> Char
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