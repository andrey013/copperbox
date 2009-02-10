{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Post
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Post - PostScript
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Post where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Parse
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils

import Text.ZParse

import Control.Applicative
import Data.Word

import Text.PrettyPrint.Leijen ( Pretty(..) )

data PostTable = PostTable { 
      post_version        :: Fixed,
      italic_angle        :: Fixed,
      underline_position  :: FWord,
      underline_thickness :: FWord,
      is_fixed_pitch      :: Word32,      -- c.f. IntegralBool
      min_mem_type42      :: Word32,
      max_mem_type42      :: Word32,
      min_mem_type1       :: Word32,
      max_mem_type1       :: Word32
    }
  deriving (Eq,Show)

readPostTable :: Monad m => ReadData m PostTable
readPostTable = PostTable <$> 
        fixed   <*> fixed   <*> fword   <*> fword  
    <*> ulong   <*> ulong   <*> ulong   <*> ulong   <*> ulong 

    
    
instance Pretty PostTable where
  pretty t = ppTable "Post Table"
      [ field "version"             24 (pretty   $ post_version t)
      , field "italic_angle"        24 (pretty   $ italic_angle t)
      , field "underline_position"  24 (pretty   $ underline_position t)
      , field "underline_thickness" 24 (pretty   $ underline_thickness t)
      , field "is_fixed_pitch"      24 (pretty   $ boolValue $ is_fixed_pitch t)
      , field "min_mem_type42"      24 (integral $ max_mem_type1 t) 
      , field "max_mem_type42"      24 (integral $ max_mem_type1 t) 
      , field "min_mem_type1"       24 (integral $ max_mem_type1 t) 
      , field "max_mem_type1"       24 (integral $ max_mem_type1 t) 
      ]

                            