
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

module ChalkboardAdaptor where

import Graphics.Chalkboard.Color
import Graphics.Chalkboard.Board
import Graphics.Chalkboard.Array ( boardToArray, arrayToBoard )

import Graphics.ZBitmap
import Graphics.ZBitmap.InternalBitmap 
import Graphics.ZBitmap.Utils
 
import Control.Monad.ST
import Data.Array
import qualified Data.Array.MArray as MA
import Data.Array.ST      ( STUArray, runSTUArray, STArray, runSTArray )
import qualified Data.Array.Unboxed as U
import Data.Foldable ( foldlM )
import Data.Word

writeBMP :: String -> (Int, Int) -> Board RGB -> IO ()
writeBMP filename (x_dim,y_dim) img = 
    writeBmp filename $ make24bitBmp $ toBitmapData arr
  where
    arr   :: Array (Int,Int) RGB
    arr   = boardToArray (x_dim-1,y_dim-1) 3 img


readBMP :: String -> IO (Board (Maybe RGB),(Int,Int))
readBMP filename = readBmp filename >>= return . convert where
    convert :: BmpBitmap -> (Board (Maybe RGB),(Int,Int))
    convert bmp = (board,(w,h)) where
        ubmp :: UniBitmap
        ubmp    = uniBitmap bmp
        
        (w,h)   = dimensions ubmp
        pixelAt = colourAt' ubmp 
        
        board :: Board (Maybe RGB)
        board   = arrayToBoard $ runSTArray $ do
            marr <- MA.newArray ((0,0),(w-1, h-1)) (RGB 0 0 0)
            foldlM (f marr) () $ cstyle2DindexList w h 
            return marr
        
        f :: STArray s (Int,Int) RGB -> () -> (Int,Int) ->  ST s ()
        f marr () idx = let colour = toRGB $ pixelAt idx in do
            MA.writeArray marr idx colour


toBitmapData ::  Array (Int,Int) RGB -> Bitmap Image24bit
toBitmapData arr = bitmap24bit width surface
  where
    (width,height)  = let ((x0,y0),(x1,y1)) = bounds arr in (1+x1-x0,1+y1-y0) 
    
    storage_sz      = 4 * ((24 * width + 31) `div` 32)
    
    surface         = runSTUArray $ do
        -- error $ show (width,height)
        marr <- MA.newArray ((0,0),(height-1, storage_sz-1)) 0
        foldlM (f marr) () $ cstyle2DindexList width height 
        return marr

  
    f :: STUArray s (Int,Int) Word8 -> () -> (Int,Int) ->  ST s ()
    f marr () (x,y) = let (rd,gn,bl) = fromRGB $ arr!(x,y) in do
        MA.writeArray marr (y,x*3)   rd
        MA.writeArray marr (y,x*3+1) gn 
        MA.writeArray marr (y,x*3+2) bl
                         
    -- bitmaps are stored upside down
    -- convert (r,c) = (height-1-r,c)


fromRGB :: RGB -> (Word8,Word8,Word8) 
fromRGB (RGB r g b) = 
    (floor $ r * 255.0, floor $ g * 255.0, floor $ b * 255.0)

toRGB :: (Word8,Word8,Word8) -> RGB
toRGB (r,g,b) = RGB (f r) (f g) (f b) where
  f = (/255.0) . realToFrac
        