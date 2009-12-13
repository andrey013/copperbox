{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Parser
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Read a DLL...
--
--------------------------------------------------------------------------------

module Hurdle.Parser where

import Hurdle.Datatypes
import Hurdle.ParseMonad

import Control.Applicative
import Data.Word
import System.IO


readDLL :: FilePath -> IO Image
readDLL filename = do 
    ans <- runParser dllFile filename
    case ans of 
      Left err -> putStrLn err >> error "readDLL failed"
      Right mf -> return mf

    

--------------------------------------------------------------------------------
-- 
    
    
dllFile :: Parser Image
dllFile = do
    dosH    <- imageDOSHeader
    toNewExeHeader (idh_new_exe_header_addr dosH)
    sig    <- signature
    coffH  <- imageCOFFHeader
    optH   <- imageOptionalHeader
    secHs  <- count (fromIntegral $ ich_num_sections coffH) sectionHeader
    expH   <- exportSectionHeader secHs
    jumpto (fromIntegral $ sh_ptr_raw_data expH)
    expD   <- exportData expH
    return $ Image { image_dos_header       = dosH
                   , image_signature        = sig
                   , image_coff_header      = coffH
                   , image_opt_header       = optH
                   , image_section_headers  = secHs
                   , image_export_data      = expD
                   }

-- bit crummy...
exportSectionHeader :: [SectionHeader] -> Parser SectionHeader
exportSectionHeader (_:_:_:_:edata:_) = return $ edata
exportSectionHeader _                 = reportFail "no .edata" 


toNewExeHeader :: Word32 -> Parser ()
toNewExeHeader n = do 
    getBytes (n - dosHSize)
    return ()  
  where
    dosHSize = 0x0040

imageDOSHeader :: Parser ImageDOSHeader
imageDOSHeader = ImageDOSHeader <$> 
        magic 
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> reserved1
    <*> getWord16le
    <*> getWord16le
    <*> reserved2
    <*> getWord32le
  where
    -- | Magic number 0x5a4d

    magic :: Parser Word16
    magic = getWord16le
  
    reserved1 :: Parser (Word16,Word16,Word16,Word16)
    reserved1 = (,,,) <$> getWord16le <*> getWord16le 
                      <*> getWord16le <*> getWord16le

    reserved2 :: Parser [Word16]
    reserved2 = count 10 getWord16le


signature :: Parser (Char,Char,Char,Char) 
signature = (,,,) <$> getChar8bit <*> getChar8bit 
                  <*> getChar8bit <*> getChar8bit

imageCOFFHeader :: Parser ImageCOFFHeader
imageCOFFHeader = ImageCOFFHeader <$> 
        getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
      

imageOptionalHeader :: Parser ImageOptionalHeader
imageOptionalHeader = ImageOptionalHeader <$> 
        imageOptionalStandard
    <*> imageOptionalNTSpecific
    <*> count 16 imageDataDirectory
    

imageOptionalStandard :: Parser ImageOptionalStandard
imageOptionalStandard = ImageOptionalStandard <$>
        getWord16le
    <*> getWord8
    <*> getWord8
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

imageOptionalNTSpecific :: Parser ImageOptionalNTSpecific
imageOptionalNTSpecific = ImageOptionalNTSpecific <$>
        getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le


imageDataDirectory :: Parser ImageDataDirectory
imageDataDirectory = ImageDataDirectory <$>
        getWord32le
    <*> getWord32le


sectionHeader :: Parser SectionHeader
sectionHeader = SectionHeader <$>
        count 8 getChar8bit
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le


exportData :: SectionHeader -> Parser ExportData
exportData section = do
    edt        <- exportDirectoryTable
    let eac    = fromIntegral $ edt_num_addr_table_entries edt
    let ea_rva = fromIntegral $ 
                   rvaToOffset (edt_export_addr_table_rva edt) section
    jumpto ea_rva
    ats        <- count eac exportAddress

    let enc    = fromIntegral $ edt_num_name_ptrs edt
    let en_rva = fromIntegral $ 
                   rvaToOffset (edt_name_ptr_table_rva edt) section
    jumpto en_rva
    nptrs      <- count enc getWord32le

    let eo_rva = fromIntegral $ 
                   rvaToOffset (edt_ordinal_table_rva edt) section
    jumpto eo_rva
    ords      <- count enc getWord16le

    names <- exportNames section nptrs

    return $ ExportData { ed_directory_table      = edt
                        , ed_export_address_table = ats
                        , ed_name_ptr_table       = nptrs
                        , ed_ordinal_table        = ords
                        , ed_name_table           = names
                        }

exportDirectoryTable :: Parser ExportDirectoryTable
exportDirectoryTable = ExportDirectoryTable <$>
        getWord32le
    <*> getWord32le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le
    <*> getWord32le

-- WRONG (for now) 
exportAddress :: Parser ExportAddress
exportAddress = EA_Export_RVA <$>
        getWord32le

exportNames :: SectionHeader -> [Word32] -> Parser [String]
exportNames _       []     = return []
exportNames section (x:xs) = mf <:> exportNames section xs
  where
    mf = jumpto (fromIntegral $ rvaToOffset x section) >> cstring


rvaToOffset :: Word32 -> SectionHeader -> Word32
rvaToOffset rva section = 
    rva - (sh_virtual_addr section - sh_ptr_raw_data section)
