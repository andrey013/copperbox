
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmapCB.ChalkboardAdaptor
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Load and save bmp bitmap files with Chalkboard via ZBitmap
--
--
--------------------------------------------------------------------------------

module Graphics.ZBitmapCB.ChalkboardAdaptor where

import Graphics.Chalkboard.Color
import Graphics.Chalkboard.Types
import Graphics.Chalkboard.Board
import Graphics.Chalkboard.Array ( boardToArray, arrayToBoard )

import Graphics.ZBitmap
import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.ZBitmap
import Graphics.ZBitmap.Utils
 
import Data.Array
import qualified Data.Array.Unboxed as U
import Data.Word

writeBMP :: String -> (Int, Int) -> Board RGB -> IO ()
writeBMP filename (x_dim,y_dim) img = 
    writeBmp filename $ zbitmapToBmp24 $ toZBitmap arr
  where
    arr   :: Array (Int,Int) RGB
    arr   = boardToArray (x_dim-1,y_dim-1) 3 img


readBMP :: String -> IO (Board RGB,(Int,Int))
readBMP _filename = do
  error $ "todo"
{-  
  a <- readBmp filename
  let b = convertBmp a
  let (width,height) = (bitmap_width b,bitmap_height b)
  return $ (arrayToBoard $ bitmap_surface b,(width,height))

-} 

toZBitmap ::  Array (Int,Int) RGB -> ZBitmap
toZBitmap arr = ZBitmap width height sw surface
  where
    ((x0,y0),(x1,y1)) = bounds arr
    width             = x1 - x0
    height            = y1 - y0
    sw                = physicalWidth B32_TrueColour width
    surface           = U.array ((0,0), (height+1,sw)) $ concat $ 
        [ let (r,g,b) = fromRGB (arr!(x,y)) in 
            [ ((y1-y,4*x),r), ((y1-y,4*x+1),g), ((y1-y,4*x+2),b), ((y1-y,4*x+3),0)] 
        | x <- [0..width]
        , y <- [0..height]
        ]

fromRGB :: RGB -> (Word8,Word8,Word8) 
fromRGB (RGB r g b) = 
    (floor $ r * 255.0, floor $ g * 255.0, floor $ b * 255.0)

        