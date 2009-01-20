{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.Util.Bitmap
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Helper for marshalling bitmaps between Haskell and FreeType
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.Util.Bitmap where

import qualified Graphics.Rendering.FreeType.Util.Array as FtArr

import Control.Monad ( when )
import Data.Array.ST ( runSTUArray )
import qualified Data.Array.MArray as MutA
import Data.Array.Unboxed ( IArray, Ix, UArray, bounds, (!) )
import Data.Foldable ( foldl', foldlM ) 
import Data.Int
import Data.Word
import Foreign.C.Types ( CUChar, CChar )

newtype Image = Image { getImage :: ImageData }

type ImageData = UArray (Int32,Int32) Word8


makeImage :: Int32 -> Int32 -> Image
makeImage w h = Image $ runSTUArray $ MutA.newArray ((0,0),(w,h)) 0

overlay :: Int32 -> Int32 -> Int32 -> [CUChar] -> Image -> Image 
overlay xpos ypos bwidth buffer (Image uarr) = Image $ runSTUArray $ do 
    arr <- MutA.thaw uarr
    foldlM (updateArr arr) (xpos,ypos) buffer 
    return arr
  where    
    updateArr a idx@(x,y) e = do 
        when ((x >= x0 && x < x1) && (y >= y0 && y < y1)) 
             (MutA.writeArray a idx (fromIntegral e)) 
        return $ incrIdx idx
    
    incrIdx (x,y) | x == bwidth   = (0,y+1)
                  | otherwise     = (x+1,y)
    
    ((x0,y0),(x1,y1)) = bounds uarr
          
          
imageBounds :: Image -> ((Int32,Int32),(Int32,Int32))
imageBounds (Image uarr) = bounds uarr

rowwise :: (Word8 -> a -> a) -> a -> Image -> a
rowwise f a (Image uarr) = FtArr.rowwise f a uarr

rowwiseM :: Monad m => (Word8 -> a -> m a) -> a -> Image -> m a
rowwiseM f a (Image uarr) = FtArr.rowwiseM f a uarr
              
colwise :: (Word8 -> a -> a) -> a -> Image -> a
colwise f a (Image uarr) = FtArr.colwise f a uarr

colwiseM :: Monad m => (Word8 -> a -> m a) -> a -> Image -> m a
colwiseM f a (Image uarr) = FtArr.colwiseM f a uarr

        
rowwiseOpa :: (Word8 -> a -> a) -> (a -> b -> b) -> (a,b) -> Image -> b
rowwiseOpa f h (a,b) (Image uarr) = FtArr.rowwiseOpa f h(a,b) uarr

rowwiseOpaM :: Monad m => 
               (Word8 -> a -> m a) -> (a -> b -> m b) -> (a,b) -> Image -> m b
rowwiseOpaM f h (a,b) (Image uarr) = FtArr.rowwiseOpaM f h (a,b) uarr
              
colwiseOpa :: (Word8 -> a -> a) -> (a -> b -> b) -> (a,b) -> Image -> b
colwiseOpa f h (a,b) (Image uarr) = FtArr.colwiseOpa f h (a,b) uarr

colwiseOpaM :: Monad m => 
               (Word8 -> a -> m a) -> (a -> b -> m b) -> (a,b) -> Image -> m b
colwiseOpaM f h (a,b) (Image uarr) = FtArr.colwiseOpaM f h (a,b) uarr
                  
                  


    

        