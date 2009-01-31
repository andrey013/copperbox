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
-- > runhaskell -i../src -L. -lfreetype Example1.hs FONT.TTF helloworld > out.txt

module Main where

import Graphics.Rendering.FreeType
import Graphics.Rendering.FreeType.Util.Bitmap

import Control.Monad
import Data.Array.IArray ( (!) )
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
drawBitmap (Bitmap r' w' _ bs _ _ _ _) x y img =
    overlay x y (fromIntegral w) (makeBuffer w r bs) img     
  where
    (r,w) = (fromIntegral r', fromIntegral w')  

showImage :: Image -> IO ()
showImage (Image img) = mapM_ f indexes 
  where
    indexes = [(r,c) | r <- [0..(image_height-1)], c <- [0..(image_width-1)] ]

    f idx@(_,c) = do show1 $ img!idx
                     when (c == image_width-1) (putChar '\n') 
    
    show1 i | i == 0    = putChar ' '
            | i <  128  = putChar '+'
            | otherwise = putChar '*'

    
    
    
main :: IO ()
main = do 
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
      setCharSize fc (f26d6 (50::Int) (64::Int)) 0 100 0

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

foldStep :: Face -> Matrix -> FoldState -> Char -> IO FoldState
foldStep fc mx (pen,image) ch = do
    setTransform fc (Just mx) pen
    loadChar fc (ord ch) [Render]
    withGlyphSlot fc $ \gs -> do 
        bleft <- bitmapLeft gs
        btop  <- bitmapTop gs
        withGSBitmap gs $ \bmp -> do 
            let img' = drawBitmap bmp bleft (target_height - btop) image 
            adv  <- advance gs
            return (moveVec pen adv, img')  
  where
    moveVec :: Vector -> Vector -> Vector
    moveVec (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

    