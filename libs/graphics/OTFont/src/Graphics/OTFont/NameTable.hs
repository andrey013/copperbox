--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.NameTable
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- NameTable
--
--------------------------------------------------------------------------------


module Graphics.OTFont.NameTable where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Utils

import qualified Data.ByteString as BS
import Data.Char ( chr ) 
import Data.List ( find )


data PlatformId = 
      Unicode
    | Macintosh
    | ISO
    | Windows
    | Custom
  deriving (Enum,Eq,Ord,Show)
  
data EncodingId = 
      Unicode_1_0
    | Unicode_1_1
    | ISO_IEC_10646
    | Unicode_2_0_BMP
    | Unicode_2_0_full
  deriving (Enum,Eq,Ord,Show)


data NameId = 
      Copyright_notice
    | Font_family_name
    | Font_subfamily_name
    | Unique_font_id
    | Full_font_name  
    | Version_string
    | PostScript_name
    | Trademark
    | Manufacturer_name
    | Designer_name
    | Description_text
    | Vendor_URL
    | Designer_URL
    | License_description
    | License_info_URL
    | Reserved_as_ZERO
    | Preferred_family
    | Preferred_subfamily
    | Compatible_full
    | Sample_text
    | PostScipt_CID
    | Reserved_name Int
  deriving (Eq,Ord,Show)

instance Enum NameId where
   fromEnum Copyright_notice    =  0
   fromEnum Font_family_name    =  1
   fromEnum Font_subfamily_name =  2
   fromEnum Unique_font_id      =  3
   fromEnum Full_font_name      =  4
   fromEnum Version_string      =  5
   fromEnum PostScript_name     =  6
   fromEnum Trademark           =  7
   fromEnum Manufacturer_name   =  8
   fromEnum Designer_name       =  9
   fromEnum Description_text    = 10
   fromEnum Vendor_URL          = 11
   fromEnum Designer_URL        = 12
   fromEnum License_description = 13
   fromEnum License_info_URL    = 14
   fromEnum Reserved_as_ZERO    = 15
   fromEnum Preferred_family    = 16
   fromEnum Preferred_subfamily = 17
   fromEnum Compatible_full     = 18
   fromEnum Sample_text         = 19
   fromEnum PostScipt_CID       = 20
   fromEnum (Reserved_name i)   = i
   
   
   toEnum  0 = Copyright_notice
   toEnum  1 = Font_family_name
   toEnum  2 = Font_subfamily_name
   toEnum  3 = Unique_font_id
   toEnum  4 = Full_font_name  
   toEnum  5 = Version_string
   toEnum  6 = PostScript_name
   toEnum  7 = Trademark
   toEnum  8 = Manufacturer_name
   toEnum  9 = Designer_name
   toEnum 10 = Description_text
   toEnum 11 = Vendor_URL
   toEnum 12 = Designer_URL
   toEnum 13 = License_description
   toEnum 14 = License_info_URL
   toEnum 15 = Reserved_as_ZERO
   toEnum 16 = Preferred_family
   toEnum 17 = Preferred_subfamily
   toEnum 18 = Compatible_full
   toEnum 19 = Sample_text
   toEnum 20 = PostScipt_CID
   toEnum  i = Reserved_name i 

  
meaning :: NameId -> String
meaning Copyright_notice    = "Copyright notice"
meaning Font_family_name    = "Font family name"
meaning Font_subfamily_name = "Font subfamily name"
meaning Unique_font_id      = "Unique font id"
meaning Full_font_name      = "Full font name"
meaning Version_string      = "Version string"
meaning PostScript_name     = "PostScript name"
meaning Trademark           = "Trademark"
meaning Manufacturer_name   = "Manufacturer name"
meaning Designer_name       = "Designer name"
meaning Description_text    = "Description text"
meaning Vendor_URL          = "Vendor URL"
meaning Designer_URL        = "Designer URL"
meaning License_description = "License description"
meaning License_info_URL    = "License info URL"
meaning Reserved_as_ZERO    = "Reserved"
meaning Preferred_family    = "Preferred family"
meaning Preferred_subfamily = "Preferred subfamily"
meaning Compatible_full     = "Compatible full"
meaning Sample_text         = "Sample text"
meaning PostScipt_CID       = "PostScipt CID findfont name"
meaning (Reserved_name i)   = "Reserved " ++ show i 
  
allMeanings :: NameTable -> [String]
allMeanings (NameTable _ _ _ ns _) = 
    map (meaning . toEnum . fromIntegral . name_id) ns 

extractText :: NameRecord -> StringData -> String
extractText (NameRecord _ _ _ _ l o) s = 
    map (chr . fromIntegral) 
          $ BS.unpack 
          $ section (fromIntegral o) (fromIntegral l) s


-- By itself, NameId is not unique           
getMeaning :: NameId -> NameTable -> Maybe String
getMeaning nid (NameTable _ _ _ ns sdata) = 
    maybe Nothing sk $ find ((== nid) . toEnum . fromIntegral . name_id) ns
  where
    sk = Just . (extractText `flip` sdata)          
                                           