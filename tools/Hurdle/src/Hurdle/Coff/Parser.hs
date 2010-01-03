{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Coff.Parser
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Read a DLL or an .o in COFF format...
--
--------------------------------------------------------------------------------

module Hurdle.Coff.Parser where

import Hurdle.Coff.Datatypes
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

{-
logAdvance :: RegionCoda -> Int -> Parser a -> Parser a
logAdvance coda dist p = 
    logPosition ("advance ("  ++ show dist ++ ") ") >> advance coda dist p
-}    

--------------------------------------------------------------------------------
-- 
    
    
dllFile :: Parser Image
dllFile = do
    dos_header          <- dosHeader
    advanceToNewExeHeader (dh_new_exe_addr dos_header)
    signature           <- pecoffSignature
    coff_header         <- coffHeader
    optional_header     <- imageOptionalHeader
    section_headers     <- sectionHeaders 
                             (fromIntegral $ ch_num_sections coff_header)
    logline "just before export data"
    mb_export_data      <- optExportData section_headers
    return $ Image { image_dos_header       = dos_header
                   , image_signature        = signature
                   , image_coff_header      = coff_header
                   , image_opt_header       = optional_header
                   , image_section_headers  = section_headers
                   , image_export_data      = mb_export_data
                   }

optExportData :: SectionHeaders -> Parser (Maybe ExportData)
optExportData = maybe (return Nothing) sk . Map.lookup ".edata" 
  where
    sk a = let raw_data = fromIntegral $ sh_ptr_raw_data a in
           liftM Just $ advance "export data" Alfermata raw_data (exportData a) 



advanceToNewExeHeader :: Word32 -> Parser ()
advanceToNewExeHeader n = do 
    _anon <- getBytes (n - dosHSize)
    return ()  
  where
    dosHSize = 0x0040

dosHeader :: Parser DOSHeader
dosHeader = do  
    magic               <- magicNumber
    bytes_on_last_page  <- word16le
    pages_in_file       <- word16le
    relocations         <- word16le
    header_paras_size   <- word16le
    min_extra_paras     <- word16le
    max_extra_paras     <- word16le
    initial_ss_value    <- word16le
    initial_sp_value    <- word16le
    checksum            <- word16le
    initial_ip_value    <- word16le
    initial_cs_value    <- word16le
    reloc_table_addr    <- word16le
    overlay_number      <- word16le
    reserved_1          <- reserved1
    oem_id              <- word16le
    oem_info            <- word16le
    reserved_2          <- reserved2
    new_exe_addr        <- word32le
    return $ DOSHeader 
                { dh_magic_number           = magic
                , dh_bytes_on_last_page     = bytes_on_last_page
                , dh_pages_in_file          = pages_in_file
                , dh_relocations            = relocations
                , dh_header_paras_size      = header_paras_size
                , dh_min_extra_paras        = min_extra_paras
                , dh_max_extra_paras        = max_extra_paras
                , dh_initial_relative_ss    = initial_ss_value
                , dh_initial_sp             = initial_sp_value
                , dh_header_checksum        = checksum
                , dh_initial_ip             = initial_ip_value
                , dh_initial_relative_cs    = initial_cs_value
                , dh_reltable_file_addr     = reloc_table_addr
                , dh_overlay_number         = overlay_number
                , dh_reserved_words         = reserved_1
                , dh_oem_identifier         = oem_id
                , dh_oem_info               = oem_info
                , dh_reserved_words_two     = reserved_2
                , dh_new_exe_addr           = new_exe_addr
                }
  where
    -- | Magic number 0x5a4d

    magicNumber :: Parser Word16
    magicNumber = word16le
  
    reserved1 :: Parser (Word16,Word16,Word16,Word16)
    reserved1 = (,,,) <$> word16le <*> word16le 
                      <*> word16le <*> word16le

    reserved2 :: Parser [Word16]
    reserved2 = count 10 word16le


pecoffSignature :: Parser (Char,Char,Char,Char) 
pecoffSignature = (,,,) <$> char <*> char 
                        <*> char <*> char



coffHeader :: Parser COFFHeader
coffHeader = do
    machine             <- word16le
    num_sections        <- word16le
    timestamp           <- word32le
    symtab_ptr          <- word32le
    num_symbols         <- word32le
    opt_header_size     <- word16le
    characteristics     <- word16le
    return $ COFFHeader 
                { ch_machine            = machine
                , ch_num_sections       = num_sections
                , ch_timedate_stamp     = timestamp
                , ch_sym_table_ptr      = symtab_ptr
                , ch_num_symbols        = num_symbols
                , ch_opt_header_size    = opt_header_size
                , ch_characteristics    = characteristics
                }      

imageOptionalHeader :: Parser ImageOptionalHeader
imageOptionalHeader = do  
    standard_fields     <- optionalStandardHeader
    nt_specific_fields  <- optionalWindowsHeader
    data_directory      <- count 16 headerDataDirectory
    return $ ImageOptionalHeader
                { ioh_header_std_fields       = standard_fields
                , ioh_nt_specific_fields      = nt_specific_fields
                , ioh_data_directory          = data_directory
                }    

optionalStandardHeader :: Parser OptionalStandardHeader
optionalStandardHeader = do
    magic               <- word16le
    major_linker_number <- word8
    minor_linker_number <- word8
    code_size           <- word32le
    initialized_dsize   <- word32le
    uninitialized_dsize <- word32le
    entry_point_addr    <- word32le
    base_of_code        <- word32le
    base_of_data        <- word32le
    return $ OptionalStandardHeader
                { osh_magic                   = magic
                , osh_major_linker_version    = major_linker_number
                , osh_minor_linker_version    = minor_linker_number
                , osh_size_of_code            = code_size
                , osh_size_of_inited_data     = initialized_dsize
                , osh_size_of_uninited_data   = uninitialized_dsize
                , osh_entry_point_addr        = entry_point_addr
                , osh_base_of_code            = base_of_code
                , osh_base_of_data            = base_of_data
                }

optionalWindowsHeader :: Parser OptionalWindowsHeader
optionalWindowsHeader = do
    image_base          <- word32le
    section_alignment   <- word32le
    file_alignment      <- word32le
    major_os_version    <- word16le
    minor_os_version    <- word16le
    major_image_version <- word16le
    minor_image_version <- word16le
    major_subsys        <- word16le
    minor_subsys        <- word16le
    win32_version       <- word32le
    image_size          <- word32le
    headers_size        <- word32le
    checksum            <- word32le
    subsystem           <- word16le
    characteristics     <- word16le
    reserve_stack_size  <- word32le
    commit_stack_size   <- word32le
    reserve_heap_size   <- word32le
    commit_heap_size    <- word32le
    loader_flags        <- word32le
    rva_num_and_sizes   <- word32le
    return $ OptionalWindowsHeader
                { owh_image_base              = image_base
                , owh_section_alignment       = section_alignment
                , owh_file_alignment          = file_alignment
                , owh_major_os_version        = major_os_version
                , owh_minor_os_version        = minor_os_version
                , owh_major_image_version     = major_image_version
                , owh_minor_image_version     = minor_image_version
                , owh_major_subsys_version    = major_subsys
                , owh_minor_subsys_version    = minor_subsys
                , owh_win32_version           = win32_version
                , owh_size_of_image           = image_size
                , owh_size_of_headers         = headers_size
                , owh_checksum                = checksum
                , owh_subsystem               = subsystem
                , owh_dll_characteristics     = characteristics
                , owh_size_stack_reserve      = reserve_stack_size
                , owh_size_stack_commit       = commit_stack_size
                , owh_size_heap_reserve       = reserve_heap_size
                , owh_size_heap_commit        = commit_heap_size
                , owh_loader_flags            = loader_flags
                , owh_rva_num_and_sizes       = rva_num_and_sizes
                } 


headerDataDirectory :: Parser HeaderDataDirectory
headerDataDirectory = do
    virtual_addr        <- word32le
    size                <- word32le
    return $ HeaderDataDirectory
                { hdd_virtual_addr      = virtual_addr
                , hdd_size              = size
                }

-- should be a Map then 

sectionHeaders :: Int -> Parser SectionHeaders
sectionHeaders n = build <$> count n sectionHeader
  where
    build = foldr (\e a -> Map.insert (sh_name e) e a) Map.empty


sectionHeader :: Parser SectionHeader
sectionHeader = do
    name                <- liftM stringTruncate (count 8 char)  
                                 -- this should be a combinator

    virtual_size        <- word32le
    virtual_addr        <- word32le
    raw_data_size       <- word32le
    raw_data_ptr        <- word32le
    relocations_ptr     <- word32le
    line_nums_ptr       <- word32le
    relocations_count   <- word16le
    line_nums_count     <- word16le
    characteristics     <- word32le
    return $ SectionHeader
                { sh_name               = name
                , sh_virtual_size       = virtual_size
                , sh_virtual_addr       = virtual_addr
                , sh_size_raw_data      = raw_data_size
                , sh_ptr_raw_data       = raw_data_ptr
                , sh_ptr_relocations    = relocations_ptr
                , sh_ptr_linenums       = line_nums_ptr
                , sh_num_relocations    = relocations_count
                , sh_num_linenums       = line_nums_count
                , sh_characteristics    = characteristics
                }


forwardParse :: RegionName 
             -> (ExportDirectoryTable -> Word32)
             -> ExportDirectoryTable
             -> SectionHeader
             -> Parser a
             -> Parser a
forwardParse name f edt section p = advance name Dalpunto pos p
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

    ats       <- forwardParse "address table" 
                              edt_export_addr_table_rva edt section
                              (exportAddressTable eac)

    logPosition "ptr_table"
    let enc   = fromIntegral $ edt_num_name_ptrs edt
    nptrs     <- forwardParse "name pointers"
                              edt_name_ptr_table_rva edt section
                              (count enc word32le)

    ords      <- forwardParse "ordinals"
                              edt_ordinal_table_rva edt section
                               (count enc word16le)

    names     <- exportNames section nptrs
    
    dllname   <- forwardParse "dll name"
                              edt_name_rva edt section cstring

    return $ ExportData 
                { ed_directory_table      = edt
                , ed_export_address_table = ats
                , ed_name_ptr_table       = nptrs
                , ed_ordinal_table        = ords
                , ed_dll_name             = dllname
                , ed_name_table           = names
                }

exportDirectoryTable :: Parser ExportDirectoryTable
exportDirectoryTable = do
    export_flags        <- word32le
    timestamp           <- word32le
    major_version       <- word16le
    minor_version       <- word16le
    name_rva            <- word32le
    ordinal_base        <- word32le
    addr_table_count    <- word32le
    name_ptrs_count     <- word32le
    export_table_rva    <- word32le
    name_table_rva      <- word32le
    ordinal_table_rva   <- word32le
    return $ ExportDirectoryTable      
                { edt_export_flags            = export_flags
                , edt_timedate_stamp          = timestamp
                , edt_major_version           = major_version
                , edt_minor_version           = minor_version
                , edt_name_rva                = name_rva
                , edt_ordinal_base            = ordinal_base
                , edt_num_addr_table_entries  = addr_table_count
                , edt_num_name_ptrs           = name_ptrs_count
                , edt_export_addr_table_rva   = export_table_rva
                , edt_name_ptr_table_rva      = name_table_rva
                , edt_ordinal_table_rva       = ordinal_table_rva
                }



--    `substError` "error - export directory table"


exportAddressTable :: Int -> Parser ExportAddressTable
exportAddressTable n = liftM ExportAddressTable $ count n exportAddress 

-- WRONG (for now) 
exportAddress :: Parser ExportAddress
exportAddress = liftM EA_Export_RVA word32le


exportNames :: SectionHeader -> [Word32] -> Parser [String]
exportNames _       []     = return []
exportNames section (x:xs) = mf <:> exportNames section xs
  where
    mf = jumpto (fromIntegral $ rvaToOffset x section) cstring
         `substError` "export name..."


jumpto :: Int -> Parser a -> Parser a
jumpto i p = 
    logline ("jumpto " ++ show i) >> advance "export name" Dalpunto i p

         
rvaToOffset :: Word32 -> SectionHeader -> Word32
rvaToOffset rva section = 
    rva - (sh_virtual_addr section - sh_ptr_raw_data section)
