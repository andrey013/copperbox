{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Datatypes
--
--------------------------------------------------------------------------------

module Graphics.OTFont.Datatypes where

import Graphics.OTFont.ParseMonad ( Region )

import qualified Data.ByteString as BS
import Data.Int
import qualified Data.Map as Map
import Data.Time.Clock
import Data.Word





--------------------------------------------------------------------------------
-- Aliases

type Byte   = Word8

-- This is a @Char@ in terms of the OFF, naturally we use something else
type SByte  = Int8 

type UShort = Word16
type Short  = Int16 
    
type ULong  = Word32
type Long   = Int32

type Offset = Word16


--------------------------------------------------------------------------------
-- Numbers - these will all need sorting out at some point

-- 16.16 float
newtype Fixed = Fixed { unFixed :: Double }
  deriving (Eq,Ord,Num, Fractional)

instance Show Fixed where 
  show = show . unFixed
    
instance Read Fixed where
  readsPrec i s = map (\(d,r) -> (Fixed d,r)) $ readsPrec i s      


newtype FWord = FWord { unFWord :: Int16 }
  deriving (Eq,Ord,Num)

instance Show FWord where 
  show = show . unFWord
    
instance Read FWord where
  readsPrec i s = map (\(d,r) -> (FWord d,r)) $ readsPrec i s   


newtype UFWord = UFWord { unUFWord :: Word16 }
  deriving (Eq,Ord,Num)

instance Show UFWord where 
  show = show . unUFWord
    
instance Read UFWord where
  readsPrec i s = map (\(d,r) -> (UFWord d,r)) $ readsPrec i s  
  
--------------------------------------------------------------------------------
-- DateTime - needs sorting out at some point

data DateTime = DateTime Word64 UTCTime

instance Show DateTime where
  show (DateTime i _) = show i
  
instance Eq DateTime where
  DateTime i _ == DateTime j _ = i == j
  
--------------------------------------------------------------------------------
--
type TableRegions = Map.Map String Region

data ProtoFace = ProtoFace {
      offset_table    :: OffsetTable,
      table_dirs      :: [TableDirectory],
      table_offsets   :: TableRegions
    }
  deriving (Eq,Show) 
  
  
data OffsetTable = OffsetTable {
      sfnt_version    :: String,
      num_tables      :: Word16,
      search_range    :: Word16,
      entry_selector  :: Word16,
      range_shift     :: Word16
    } 
  deriving (Eq,Show)      
      
data TableDirectory = TableDirectory {
      tag             :: String,
      check_sum       :: Word32,
      offset          :: Word32,
      table_length    :: Word32
    } 
  deriving (Eq,Show)  
 

 

type StringData = BS.ByteString
  



  
  

    