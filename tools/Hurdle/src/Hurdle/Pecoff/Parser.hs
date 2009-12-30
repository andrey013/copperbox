{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Pecoff.Parser
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

module Hurdle.Pecoff.Parser where

import Hurdle.Pecoff.Datatypes
import Hurdle.Base.Utils

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Data.Word


readDLL :: FilePath -> IO Image
readDLL filename = do
    (ans,w) <- runKangaroo dllFile filename
    case ans of 
      Left err -> (putStrLn $ toList w) >> error err
      Right mf -> return mf


readCOFF :: FilePath -> IO COFFHeader
readCOFF filename = do
    (ans,w) <- runKangaroo coffHeader filename
    case ans of 
      Left err -> (putStrLn $ toList w) >> error err
      Right mf -> return mf



--------------------------------------------------------------------------------
-- 
    
    
dllFile :: Parser Image
dllFile = do
    dosH    <- imageDOSHeader
    toNewExeHeader (idh_new_exe_header_addr dosH)
    sig    <- signature
    coffH  <- coffHeader
    optH   <- imageOptionalHeader
    secHs  <- sectionHeaders (fromIntegral $ ch_num_sections coffH)
    opt_expos <- optExportData secHs
    return $ Image { image_dos_header       = dosH
                   , image_signature        = sig
                   , image_coff_header      = coffH
                   , image_opt_header       = optH
                   , image_section_headers  = secHs
                   , image_export_data      = opt_expos
                   }

optExportData :: SectionHeaders -> Parser (Maybe ExportData)
optExportData = maybe (return Nothing) sk . Map.lookup ".edata" 
  where
    sk a = let raw_data = fromIntegral $ sh_ptr_raw_data a in
           liftM Just $ advanceAlfermataAbsolute raw_data (exportData a)




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

coffHeader :: Parser COFFHeader
coffHeader = COFFHeader <$> 
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


-- should be a Map then 

sectionHeaders :: Int -> Parser SectionHeaders
sectionHeaders n = build <$> count n sectionHeader
  where
    build = foldr (\e a -> Map.insert (sh_name e) e a) Map.empty


sectionHeader :: Parser SectionHeader
sectionHeader = SectionHeader <$>
        liftM stringTruncate (count 8 char)  -- this should be a combinator
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

forwardParse :: (ExportDirectoryTable -> Word32)
             -> ExportDirectoryTable
             -> SectionHeader
             -> Parser a
             -> Parser a
forwardParse f edt section p = advanceDalpuntoAbsolute pos p
  where
    pos = fromIntegral $ rvaToOffset (f edt) section
    

-- At some point... I'll tidy this up.

exportData :: SectionHeader -> Parser ExportData
exportData section = do
    logPosition "starting exportData..."
    edt       <- exportDirectoryTable
    logline   $ show edt

    logPosition "export addr table"
    let eac   = fromIntegral $ edt_num_addr_table_entries edt
    logline   $ "num entries in export address table " ++ show eac

    ats       <- forwardParse edt_export_addr_table_rva edt section
                              (exportAddressTable eac)

    logPosition "ptr_table"
    let enc   = fromIntegral $ edt_num_name_ptrs edt
    nptrs     <- forwardParse edt_name_ptr_table_rva edt section
                              (count enc word32le)

    ords      <- forwardParse edt_ordinal_table_rva edt section
                               (count enc word16le)

    names     <- exportNames section nptrs
    
    dllname   <- forwardParse edt_name_rva edt section cstring

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
    `substError` "error - export directory table"


exportAddressTable :: Int -> Parser ExportAddressTable
exportAddressTable n = ExportAddressTable <$> count n exportAddress 

-- WRONG (for now) 
exportAddress :: Parser ExportAddress
exportAddress = EA_Export_RVA <$> word32le


exportNames :: SectionHeader -> [Word32] -> Parser [String]
exportNames _       []     = return []
exportNames section (x:xs) = mf <:> exportNames section xs
  where
    mf = jumpto (fromIntegral $ rvaToOffset x section) cstring
         `substError` "export name..."

         
rvaToOffset :: Word32 -> SectionHeader -> Word32
rvaToOffset rva section = 
    rva - (sh_virtual_addr section - sh_ptr_raw_data section)
