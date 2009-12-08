{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DLLexports.ReadFile
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

module DLLexports.ReadFile where

import DLLexports.Datatypes
import DLLexports.ParseMonad

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
    expD   <- exportData
    return $ Image { image_dos_header       = dosH
                   , image_signature        = sig
                   , image_coff_header      = coffH
                   , image_opt_header       = optH
                   , image_section_headers  = secHs
                   , image_export_data      = expD
                   }


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


exportData :: Parser ExportData
exportData = ExportData <$>
        exportDirectoryTable
    <*> pure []
    <*> pure []
    <*> pure []
    <*> pure []     

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
      
