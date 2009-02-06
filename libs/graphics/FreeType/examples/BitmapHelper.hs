{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  BitmapHelper
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

module BitmapHelper where


import Control.Monad ( when )
import Data.Bits ( (.|.) )

import Data.Array.IArray ( Array )
import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTUArray, runSTArray )
import Data.Array.Unboxed ( IArray, UArray, bounds )
import Data.Char ( ord )
import Data.Foldable ( foldlM ) 
import Data.Int
import Data.Word


newtype Image = Image { getImage :: ImageData }

type ImageData = UArray (Int32,Int32) Word8


makeImage :: Int32 -> Int32 -> Image
makeImage w h = Image $ runSTUArray $ MA.newArray ((0,0),(rows,cols)) 0
  where rows = h; cols = w
  
overlay :: Int32 -> Int32 -> Int32 -> Buffer -> Image -> Image 
overlay xpos ypos bwidth (Buffer buffer) (Image uarr) = Image $ runSTUArray $ do 
    arr <- MA.thaw uarr
    foldlM (fn arr) (ypos,xpos) buffer
    return arr
  where
    fn arr idx@(j,i) e = do
        when (inbounds j i) 
             (do val <- MA.readArray arr idx
                 MA.writeArray arr idx (val .|. e))
        return $ next idx
    
   
    next (j,i) | i+1 == xpos+bwidth = (j+1,xpos)
               | otherwise          = (j,i+1)        
            
          
    inbounds row col = (row >= minR && row <= maxR) 
                    && (col >= minC && col <= maxC)
    
    ((minR,minC),(maxR,maxC))   = bounds uarr 
        
               

type BufferData = Array Int32 Word8

newtype Buffer = Buffer { getBuffer :: BufferData }
  deriving (Show)

makeBuffer :: Int -> Int -> String -> Buffer
makeBuffer w h chars = Buffer $ runSTArray $ do 
    arr <- MA.newArray (0,height * width) 0
    step arr 0 chars 
  where
    step arr _   []     = return arr
    step arr i   (c:cs) = do MA.writeArray arr i (fromIntegral $ ord c)
                             step arr (i + 1) cs
                                    
    
    width, height :: Int32
    (width, height) = (fromIntegral w,fromIntegral h)


              


    

        