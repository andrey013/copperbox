{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  FontData
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Load a font and print some information about it...
--

--------------------------------------------------------------------------------
-- windows & cygwin with lbfreetype.dll in current directory 
-- > ghc --make -i../src -L. -lfreetype Example1.hs

-- For this example I've used Tempest.ttf available from:
-- http://www.fontfreak.com/charactermaps/t/Tempest.htm
-- and put in the directory \'./data\'

-- ghci > :set args "./data/Tempest.ttf" "helloworld"

module Main where

import Graphics.Rendering.FreeType
import Graphics.Rendering.FreeType.Util.Bitmap

import Control.Monad
import Data.Char ( ord )
import Data.Int
import Data.Word
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )


image_width, image_height :: Int32
image_width  = 640
image_height = 480

target_height :: Int32
target_height = image_height

drawBitmap :: Bitmap -> Int32 -> Int32 -> Image -> Image
drawBitmap (Bitmap _ w _ bs) x y img = overlay x y (fromIntegral w) bs img

showImage :: Image -> IO ()
showImage img = rowwiseOpaM f h ((),()) img
  where
    f i _ | i == 0    = putChar ' '
          | i <  128  = putChar '+'
          | otherwise = putChar '*'

    h _ _ = putChar '\n'           
    
    
main :: IO ()
main = do 
  putStrLn "------ Running"
  args <- getArgs
  case args of 
    [a,b] -> main2 a b >> exitSuccess
    _     -> putStrLn "usage: %s font sample-text\n" >> exitFailure
                
type Pen = Vector
type FoldState = (Pen, Image)

main2 :: FilePath -> String -> IO ()
main2 path text =
  withFreeType () $ \ft -> 
    withNewFace ft path 0 () $ \fc -> do
      setCharSize fc 0 (f26d6 (16::Int) (64::Int)) 150 150

      (showImage . snd) =<< foldM (foldStep fc matrix) (pen,image) text
  where
    image = makeImage image_width image_height 
    angle :: Double
    angle = ( 25.0 / 360 ) * 3.14159 * 2 
    maxWord16 :: Double
    maxWord16 = fromIntegral (maxBound::Word16)
                
    matrix = Matrix (floor $          (cos angle) * maxWord16)
                    (floor $ negate $ (sin angle) * maxWord16)
                    (floor $          (sin angle) * maxWord16)
                    (floor $          (cos angle) * maxWord16)
    
    pen = Vector (300 * 64) ((image_height - 200) * 64)                    

foldStep :: FT_face -> Matrix -> FoldState -> Char -> IO FoldState
foldStep fc mx (pen,image) ch = do
    setTransform fc mx pen
    loadChar fc (ord ch) [Render]
    withGlyphSlot fc $ \gs -> do 
        bleft <- bitmapLeft gs
        btop  <- bitmapTop gs
        withBitmap gs $ \bmp -> do 
            adv <- advance gs
            let img' = drawBitmap bmp bleft (target_height - btop) image            
            return (moveVec pen adv, img')  
  where
    moveVec :: Vector -> Vector -> Vector
    moveVec (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)