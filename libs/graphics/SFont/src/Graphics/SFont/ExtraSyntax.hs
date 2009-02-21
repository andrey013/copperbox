{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.ExtraSyntax
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Intermediate syntax that is used during parsing but is not retained 
-- in the /syntax tree/.
--
--------------------------------------------------------------------------------


module Graphics.SFont.ExtraSyntax where

import Graphics.SFont.PrimitiveDatatypes

import qualified Data.Map as Map


-- Read from the loca table (gylf_data lengths are synthesized)
type GlyfStartLoca = Int

-- Table locations map table names to there position in the font source file
-- (where position is a Region - start, length) 
type TableLocs = Map.Map String Region 


-- The table directory contains a list of all tables and their offset (start) 
-- and length. Here they are called TableDescriptors. 
-- TableDescriptor (table_name,table_location  
data TableDescriptor = TableDescriptor 
        { table_name        :: String 
        , table_location    :: Region 
        }
  deriving (Eq,Ord,Show)

  