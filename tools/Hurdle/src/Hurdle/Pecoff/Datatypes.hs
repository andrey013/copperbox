{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Pecoff.Datatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------

module Hurdle.Pecoff.Datatypes where

import Data.Map ( Map )
import Data.Word

data Image = Image 
      { image_dos_header            :: ImageDOSHeader
      , image_signature             :: (Char,Char,Char,Char)
      , image_coff_header           :: ImageCOFFHeader
      , image_opt_header            :: ImageOptionalHeader
      , image_section_headers       :: SectionHeaders
      , image_export_data           :: Maybe ExportData
      }
  deriving Show

image_DOS_HEADER_size :: Int
image_DOS_HEADER_size = 64

data ImageDOSHeader = ImageDOSHeader 
      { idh_magic_number            :: Word16
      , idh_bytes_last_page         :: Word16
      , idh_pages_in_file           :: Word16
      , idh_relocations             :: Word16
      , idh_size_header_paras       :: Word16
      , idh_min_extra_paras         :: Word16
      , idh_max_extra_paras         :: Word16
      , idh_initial_relative_ss     :: Word16
      , idh_initial_sp              :: Word16
      , idh_header_checksum         :: Word16
      , idh_initial_ip              :: Word16   
      , idh_initial_relative_cs     :: Word16
      , idh_reltable_file_addr      :: Word16
      , idh_overlay_number          :: Word16
      , idh_reserved_words          :: (Word16,Word16,Word16,Word16)
      , idh_oem_identifier          :: Word16
      , idh_oem_info                :: Word16
      , idh_reserved_words_two      :: [Word16]   -- length 10
      , idh_new_exe_header_addr     :: Word32
      }  
  deriving Show



image_COFF_HEADER_size :: Int
image_COFF_HEADER_size = 20

data ImageCOFFHeader = ImageCOFFHeader 
      { ich_machine                 :: Word16
      , ich_num_sections            :: Word16
      , ich_timedate_stamp          :: Word32
      , ich_sym_table_ptr           :: Word32
      , ich_num_symbols             :: Word32
      , ich_opt_header_size         :: Word16
      , ich_characteristics         :: Word16
      }
   deriving Show

image_OPTIONAL_HEADER_size :: Int
image_OPTIONAL_HEADER_size = 
    image_OPTIONAL_STANDARD_size + image_OPTIONAL_NT_SPECIFIC_size
       + 16*image_DATA_DIRECTORY_size

data ImageOptionalHeader = ImageOptionalHeader
      { ioh_header_std_fields       :: ImageOptionalStandard
      , ioh_nt_specific_fields      :: ImageOptionalNTSpecific
      , ioh_data_directory          :: [ImageDataDirectory]
      }
  deriving Show
 

image_OPTIONAL_STANDARD_size :: Int
image_OPTIONAL_STANDARD_size = 28

data ImageOptionalStandard = ImageOptionalStandard
      { ios_magic                   :: Word16
      , ios_major_linker_version    :: Word8
      , ios_minor_linker_version    :: Word8
      , ios_size_of_code            :: Word32
      , ios_size_of_inited_data     :: Word32
      , ios_size_of_uninited_data   :: Word32
      , ios_entry_point_addr        :: Word32
      , ios_base_of_code            :: Word32
      , ios_base_of_data            :: Word32
      }
   deriving Show

image_OPTIONAL_NT_SPECIFIC_size :: Int
image_OPTIONAL_NT_SPECIFIC_size = 68

data ImageOptionalNTSpecific = ImageOptionalNTSpecific
      { iont_image_base             :: Word32
      , iont_section_alignment      :: Word32
      , iont_file_alignment         :: Word32
      , iont_major_os_version       :: Word16
      , iont_minor_os_version       :: Word16
      , iont_major_image_version    :: Word16
      , iont_minor_image_version    :: Word16
      , iont_major_subsys_version   :: Word16
      , iont_minor_subsys_version   :: Word16
      , iont_win32_version          :: Word32
      , iont_size_of_image          :: Word32
      , iont_size_of_headers        :: Word32
      , iont_checksum               :: Word32
      , iont_subsystem              :: Word16
      , iont_dll_characteristics    :: Word16
      , iont_size_stack_reserve     :: Word32
      , iont_size_stack_commit      :: Word32
      , iont_size_heap_reserve      :: Word32
      , iont_size_heap_commit       :: Word32
      , iont_loader_flags           :: Word32
      , iont_rva_num_and_sizes      :: Word32
      } 
  deriving Show

image_DATA_DIRECTORY_size :: Int
image_DATA_DIRECTORY_size = 4

data ImageDataDirectory = ImageDataDirectory
      { idd_virtual_addr            :: Word32
      , idd_size                    :: Word32
      }
  deriving Show


type SectionHeaders = Map String SectionHeader

data SectionHeader = SectionHeader 
      { sh_name                     :: String  -- 8 bytes
      , sh_virtual_size             :: Word32
      , sh_virtual_addr             :: Word32
      , sh_size_raw_data            :: Word32
      , sh_ptr_raw_data             :: Word32
      , sh_ptr_relocations          :: Word32
      , sh_ptr_linenums             :: Word32
      , sh_num_relocations          :: Word16
      , sh_num_linenums             :: Word16
      , sh_characteristics          :: Word32
      }
  deriving Show

-- | \'.edata\'
-- name_ptrs and ordinals should be zipped...
data ExportData = ExportData
      { ed_directory_table          :: ExportDirectoryTable
      , ed_export_address_table     :: ExportAddressTable
      , ed_name_ptr_table           :: [Word32]
      , ed_ordinal_table            :: [Word16]
      , ed_dll_name                 :: String
      , ed_name_table               :: [String]
      }
  deriving Show

data ExportDirectoryTable = ExportDirectoryTable
      { edt_export_flags            :: Word32
      , edt_timedate_stamp          :: Word32
      , edt_major_version           :: Word16
      , edt_minor_version           :: Word16
      , edt_name_rva                :: Word32
      , edt_ordinal_base            :: Word32
      , edt_num_addr_table_entries  :: Word32
      , edt_num_name_ptrs           :: Word32
      , edt_export_addr_table_rva   :: Word32
      , edt_name_ptr_table_rva      :: Word32
      , edt_ordinal_table_rva       :: Word32
      }
  deriving Show

newtype ExportAddressTable = ExportAddressTable 
          { getExportAddressTable :: [ExportAddress] }
   deriving Show


data ExportAddress = EA_Export_RVA     Word32
                   | EA_Forwarder_RVA  Word32    
  deriving Show





