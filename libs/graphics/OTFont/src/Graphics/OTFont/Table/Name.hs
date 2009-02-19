{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Table.Name
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Name Table
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Table.Name where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty
import Graphics.OTFont.Utils
import Graphics.OTFont.Table.CommonDatatypes

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Char ( chr ) 
import Data.List ( find )
import Data.Typeable

import Text.PrettyPrint.Leijen ( Pretty(..), indent, vsep )

data NameTable = NameTable { 
      nt_format       :: UShort,
      nt_count        :: UShort,
      string_offset   :: UShort,
      name_records    :: [NameRecord],
      string_data     :: StringData
    }
  deriving (Eq,Show,Typeable)

instance Pretty NameTable where
  pretty (NameTable nf nc so ns _) = ppTable "Name Table" 
      [ field "nt_format"       16 (integral nf)
      , field "nt_count"        16 (integral nc)
      , field "string_offset"   16 (integral so)
      , field "name_records"    16 (indent 0 $ vsep (map prettyThenLine ns))
      ]
        
data NameRecord = NameRecord {
      platform_id     :: PlatformId,
      encoding_id     :: EncodingId,
      language_id     :: UShort,
      name_id         :: NameId,
      string_length   :: UShort,
      str_offset      :: UShort
    }
  deriving (Eq,Show)
  

instance Pretty NameRecord where 
  pretty (NameRecord pid ei li ni sl so) = ppTable "Name Record"
      [ field "platform_id"     16 (ppMeaning pid)
      , field "encoding_id"     16 (ppMeaning ei)
      , field "language_id"     16 (integral li)
      , field "name_id"         16 (ppMeaning ni)
      , field "string_length"   16 (integral sl)
      , field "str_offset"      16 (integral so)
      ]

      

  


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

  
instance Meaning NameId where
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

readNameTable :: Monad m => Region -> ParserT r m NameTable
readNameTable (i,j) = withinRangeAbs i j $ do 
    nf  <- ushort
    nc  <- ushort
    so  <- ushort
    nr  <- count (fromIntegral nc) nameRecord
    sd  <- runOnL word8
    return $ NameTable nf nc so nr (BS.pack sd)


nameRecord :: Monad m => ParserT r m NameRecord 
nameRecord = NameRecord <$>
   platformId <*> encodingId <*> ushort <*> nameId <*> ushort <*> ushort 
  where 
    nameId     = toEnum . fromIntegral <$> ushort
     
allMeanings :: NameTable -> [String]
allMeanings (NameTable _ _ _ ns _) = map (meaning . name_id) ns 

    
extractText :: NameRecord -> StringData -> String
extractText (NameRecord _ _ _ _ l o) s = 
    map (chr . fromIntegral) 
          $ BS.unpack 
          $ section (fromIntegral o) (fromIntegral l) s


-- By itself, NameId is not unique           
getMeaning :: NameId -> NameTable -> Maybe String
getMeaning nid (NameTable _ _ _ ns sdata) = 
    maybe Nothing sk $ find ((== nid) . name_id) ns
  where
    sk = Just . (extractText `flip` sdata)          
                                           