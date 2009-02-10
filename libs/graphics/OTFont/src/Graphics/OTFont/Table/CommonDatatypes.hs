{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.CommonDatatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common datatypes such as platform_id
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.CommonDatatypes where

import Graphics.OTFont.Parse
import Graphics.OTFont.Utils

import Text.ZParse ( BinaryParserT )

import Control.Applicative
import Data.Word

data PlatformId = 
      Unicode
    | Macintosh
    | ISO
    | Windows
    | Custom
    | PlatformId Word16
  deriving (Eq,Ord,Show)

instance Enum PlatformId where
   fromEnum Unicode         = 0
   fromEnum Macintosh       = 1
   fromEnum ISO             = 2
   fromEnum Windows         = 3
   fromEnum Custom          = 4
   fromEnum (PlatformId i)  = fromIntegral i
  
   toEnum  0 = Unicode
   toEnum  1 = Macintosh
   toEnum  2 = ISO
   toEnum  3 = Windows
   toEnum  4 = Custom  
   toEnum  i = PlatformId $ fromIntegral i
   
instance Meaning PlatformId where
  meaning Unicode         = "Unicode"
  meaning Macintosh       = "Macintosh"
  meaning ISO             = "ISO"
  meaning Windows         = "Windows"
  meaning Custom          = "Custom"
  meaning (PlatformId i)  = show i
    
data EncodingId = 
      Unicode_1_0
    | Unicode_1_1
    | ISO_IEC_10646
    | Unicode_2_0_BMP
    | Unicode_2_0_full
    | EncodingId Word16
  deriving (Eq,Ord,Show)

instance Enum EncodingId where
   fromEnum Unicode_1_0       = 0
   fromEnum Unicode_1_1       = 1
   fromEnum ISO_IEC_10646     = 2
   fromEnum Unicode_2_0_BMP   = 3
   fromEnum Unicode_2_0_full  = 4
   fromEnum (EncodingId i)    = fromIntegral i
  
   toEnum  0 = Unicode_1_0
   toEnum  1 = Unicode_1_1
   toEnum  2 = ISO_IEC_10646
   toEnum  3 = Unicode_2_0_BMP
   toEnum  4 = Unicode_2_0_full  
   toEnum  i = EncodingId $ fromIntegral i

instance Meaning EncodingId where
  meaning Unicode_1_0       = "Unicode 1.0"
  meaning Unicode_1_1       = "Unicode 1.1"
  meaning ISO_IEC_10646     = "ISO/IEC 10646 semantics"
  meaning Unicode_2_0_BMP   = "Unicode 2.0, BMP only"
  meaning Unicode_2_0_full  = "Unicode 2.0, full repetoire"
  meaning (EncodingId i)    = show i
  
platformId :: Monad m => BinaryParserT m PlatformId 
platformId = toEnum . fromIntegral <$> ushort 
      
encodingId :: Monad m => BinaryParserT m EncodingId 
encodingId = toEnum . fromIntegral <$> ushort 


data WindowsEncoding = 
      Symbol
    | WEncUnicode
    | ShiftJIS
    | PRC
    | Big5
    | Wansung
    | Johab
    | WEncReserved7
    | WEncReserved8
    | WEncReserved9
    | USC_4
  deriving (Enum,Eq,Ord,Show)

instance Meaning WindowsEncoding where
  meaning Symbol        = "Symbol"
  meaning WEncUnicode   = "Unicode"
  meaning ShiftJIS      = "ShiftJIS"
  meaning PRC           = "PRC"
  meaning Big5          = "Big5"
  meaning Wansung       = "Wansung"
  meaning Johab         = "Johab"
  meaning WEncReserved7 = "Reserved"
  meaning WEncReserved8 = "Reserved"
  meaning WEncReserved9 = "Reserved"
  meaning USC_4         = "USC-4"
  
  
      