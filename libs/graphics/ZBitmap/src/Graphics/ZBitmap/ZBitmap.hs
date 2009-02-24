{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.ZBitmap
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Internal bitmap representation
--
--------------------------------------------------------------------------------

module Graphics.ZBitmap.ZBitmap where

import Data.Array.Unboxed ( UArray )


import Data.Word 



data ZBitmap = ZBitmap { 
      bitmap_width      :: Int,
      bitmap_height     :: Int,
      surface_width     :: Int,
      bitmap_surface    :: UArray (Int,Int) Word8
    }
    

instance Show ZBitmap where
  show (ZBitmap w h sw _) = 
        "ZBitmap{ width=" ++ show w 
           ++ ", height=" ++ show h 
           ++ ", surface_width=" ++ show sw
           ++ " }"       
