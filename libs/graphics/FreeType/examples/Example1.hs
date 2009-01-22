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

drawBitmap :: Bitmap -> Int32 -> Int32 -> Image -> IO Image
drawBitmap bmp x y img = do
    bs <- getBitmapBuffer bmp
    return $ overlay x y (fromIntegral w) (makeBuffer w r bs) img     
  where
    (r,w) = (getBitmapRows bmp, getBitmapWidth bmp)  

showImage :: Image -> IO ()
showImage img = zigZagPhiM f h ((),()) img
  where
    f i () | i == 0    = putChar ' '
           | i <  128  = putChar '+'
           | otherwise = putChar '*'

    h () () = putChar '\n'           
    
    
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

foldStep :: FT_face -> Matrix -> FoldState -> Char -> IO FoldState
foldStep fc mx (pen,image) ch = do
    setTransform fc (Just mx) pen
    loadChar fc (ord ch) [Render]
    withGlyphSlot fc $ \gs -> do 
        bleft <- bitmapLeft gs
        btop  <- bitmapTop gs
        withBitmap gs $ \bmp -> do 
            img' <- drawBitmap bmp bleft (target_height - btop) image 
            adv  <- advance gs
            return (moveVec pen adv, img')  
  where
    moveVec :: Vector -> Vector -> Vector
    moveVec (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

    