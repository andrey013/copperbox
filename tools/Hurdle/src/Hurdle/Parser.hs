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
import Hurdle.Utils

import Control.Applicative
import Data.Word


readDLL :: FilePath -> IO Image
readDLL filename = do
    (ans,w) <- runKangaroo dllFile filename
    case ans of 
      Left err -> (putStrLn $ toList w) >> error err
      Right mf -> return mf




infixr 5 <:>

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


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
    let raw_data = fromIntegral $ sh_ptr_raw_data expH
    logline $ "raw_data is " ++ show raw_data
    expD   <- advanceAlfermataAbsolute raw_data (exportData expH)
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
exportSectionHeader _                 = reportError "no .edata" 


toNewExeHeader :: Word32 -> Parser ()
toNewExeHeader n = do 
    _anon <- getBytes (n - dosHSize)
    return ()  
  where
    dosHSize = 0x0040

imageDOSHeader :: Parser ImageDOSHeader
imageDOSHeader = ImageDOSHeader <$> 
        magic 
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> reserved1
    <*> word16le
    <*> word16le
    <*> reserved2
    <*> word32le
  where
    -- | Magic number 0x5a4d

    magic :: Parser Word16
    magic = word16le
  
    reserved1 :: Parser (Word16,Word16,Word16,Word16)
    reserved1 = (,,,) <$> word16le <*> word16le 
                      <*> word16le <*> word16le

    reserved2 :: Parser [Word16]
    reserved2 = count 10 word16le


signature :: Parser (Char,Char,Char,Char) 
signature = (,,,) <$> char <*> char 
                  <*> char <*> char

imageCOFFHeader :: Parser ImageCOFFHeader
imageCOFFHeader = ImageCOFFHeader <$> 
        word16le
    <*> word16le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word16le
    <*> word16le
      

imageOptionalHeader :: Parser ImageOptionalHeader
imageOptionalHeader = ImageOptionalHeader <$> 
        imageOptionalStandard
    <*> imageOptionalNTSpecific
    <*> count 16 imageDataDirectory
    

imageOptionalStandard :: Parser ImageOptionalStandard
imageOptionalStandard = ImageOptionalStandard <$>
        word16le
    <*> word8
    <*> word8
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le

imageOptionalNTSpecific :: Parser ImageOptionalNTSpecific
imageOptionalNTSpecific = ImageOptionalNTSpecific <$>
        word32le
    <*> word32le
    <*> word32le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word16le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word16le
    <*> word16le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le


imageDataDirectory :: Parser ImageDataDirectory
imageDataDirectory = ImageDataDirectory <$>
        word32le
    <*> word32le


sectionHeader :: Parser SectionHeader
sectionHeader = SectionHeader <$>
        count 8 char
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word16le
    <*> word16le
    <*> word32le


jumpto :: Int -> Parser a -> Parser a
jumpto = advanceDalpuntoAbsolute

-- At some point... I'll tidy this up.

exportData :: SectionHeader -> Parser ExportData
exportData section = do
    logPosition "starting exportData..."
    edt        <- exportDirectoryTable
    
    let eac    = fromIntegral $ edt_num_addr_table_entries edt
    let ea_rva = fromIntegral $ 
                   rvaToOffset (edt_export_addr_table_rva edt) section
    ats        <- jumpto ea_rva (count eac exportAddress)

    let enc    = fromIntegral $ edt_num_name_ptrs edt
    let en_rva = fromIntegral $ 
                   rvaToOffset (edt_name_ptr_table_rva edt) section
    nptrs      <- jumpto en_rva (count enc word32le)

    let eo_rva = fromIntegral $ 
                   rvaToOffset (edt_ordinal_table_rva edt) section
    ords      <- jumpto eo_rva (count enc word16le)

    names     <- exportNames section nptrs
    
    let nm_rva = fromIntegral $ 
                   rvaToOffset (edt_name_rva edt) section

    dllname   <- jumpto nm_rva cstring

    return $ ExportData { ed_directory_table      = edt
                        , ed_export_address_table = ats
                        , ed_name_ptr_table       = nptrs
                        , ed_ordinal_table        = ords
                        , ed_dll_name             = dllname
                        , ed_name_table           = names
                        }

exportDirectoryTable :: Parser ExportDirectoryTable
exportDirectoryTable = ExportDirectoryTable <$>
        word32le
    <*> word32le
    <*> word16le
    <*> word16le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le
    <*> word32le

-- WRONG (for now) 
exportAddress :: Parser ExportAddress
exportAddress = EA_Export_RVA <$>
        word32le


exportNames :: SectionHeader -> [Word32] -> Parser [String]
exportNames _       []     = return []
exportNames section (x:xs) = mf <:> exportNames section xs
  where
    mf = jumpto (fromIntegral $ rvaToOffset x section) cstring

         
rvaToOffset :: Word32 -> SectionHeader -> Word32
rvaToOffset rva section = 
    rva - (sh_virtual_addr section - sh_ptr_raw_data section)
