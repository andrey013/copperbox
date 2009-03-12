{-# LANGUAGE FlexibleContexts           #-}
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

module Graphics.BmpAdaptor (
  readBMP,
  writeBMP
) where

import Graphics.Chalkboard.Color
import Graphics.Chalkboard.Board
import Graphics.Chalkboard.Array ( boardToArray, arrayToBoard )

import Graphics.BmpAdaptor.InternalBitmap
import Graphics.BmpAdaptor.InternalSyntax
import Graphics.BmpAdaptor.ReadBmp
import Graphics.BmpAdaptor.Utils
import Graphics.BmpAdaptor.WriteBmp
  
import Data.Array
import qualified Data.Array.Unboxed as U
import Data.Word


{-
writeBMP :: String -> (Int, Int) -> Board RGB -> IO ()
writeBMP filename (x_dim,y_dim) img = 
    writeBmp filename $ make24bitBmp $ toBitmapData arr
  where
    arr   :: Array (Int,Int) RGB
    arr   = boardToArray (x_dim-1,y_dim-1) 3 img
-}

writeBMP :: String -> (Int, Int) -> Board RGB -> IO ()
writeBMP filename (x_dim,y_dim) img = do 
    putStrLn "one"
    let bmp = toBitmapData arr
    putStrLn $ show $ (imageWidth bmp, imageHeight bmp)
    error $ "here"
    let bmp' = make24bitBmp bmp
    putStrLn $ show  bmp' 
    writeBmp filename bmp'
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
        board   = arrayToBoard $ array ((0,0),(w-1, h-1)) $ 
            map (tup (toRGB . pixelAt)) $ cstyle2DindexList w h 

        tup :: ((Int,Int) -> b) -> (Int,Int) -> ((Int,Int),b)
        tup f (r,c) = ((c,h-1-r),f (r,c))

toBitmapData ::  Array (Int,Int) RGB -> Bitmap Image24bit
toBitmapData arr = bitmap24bit width surface
  where
    (width,height)  = let ((x0,y0),(x1,y1)) = bounds arr in (1+x1-x0,1+y1-y0) 
    
    storage_sz      = 4 * ((24 * width + 31) `div` 32)
    surface         = error $ show ((width,height), storage_sz)
{-    
    surface         = arrBuild ((0,0),(height-1, storage_sz-1)) $ concat $ 
                        map f $ cartesian2DindexList width height 
-}
    f :: (Int,Int) -> [((Int,Int),Word8)]
    f (x,y) = let (rd,gn,bl) = if (x,y) >(height-1,width-1) then error $ show (x,y) 
                                  else fromRGB $ arr!(x,y)
              in [ ((y,x*3),rd), ((y,x*3+1),gn), ((y,x*3+2),bl) ]

arrBuild :: (Ix i, U.IArray a e, Show i) => (i, i) -> [(i, e)] -> a i e
arrBuild (u,l) xs
    | all f xs    = U.array (u,l) xs
    | otherwise   = error "here"
  where
    f (i,e) = if (u,l) `inRange` i then True else error $ "AB - " ++ show i     
                         
    -- bitmaps are stored upside down
    -- convert (r,c) = (height-1-r,c)


fromRGB :: RGB -> (Word8,Word8,Word8) 
fromRGB (RGB r g b) = 
    (floor $ r * 255.0, floor $ g * 255.0, floor $ b * 255.0)

toRGB :: (Word8,Word8,Word8) -> RGB
toRGB (r,g,b) = RGB (f r) (f g) (f b) where
  f = (/255.0) . realToFrac
        