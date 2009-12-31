{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Coff.Datatypes
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

module Hurdle.Coff.Datatypes where

import Data.Map ( Map )
import Data.Word

data Image = Image 
      { image_dos_header            :: DOSHeader
      , image_signature             :: (Char,Char,Char,Char)
      , image_coff_header           :: COFFHeader
      , image_opt_header            :: ImageOptionalHeader
      , image_section_headers       :: SectionHeaders
      , image_export_data           :: Maybe ExportData
      }
  deriving Show

image_DOS_HEADER_size :: Int
image_DOS_HEADER_size = 64

data DOSHeader = DOSHeader 
      { dh_magic_number            :: Word16
      , dh_bytes_on_last_page      :: Word16
      , dh_pages_in_file           :: Word16
      , dh_relocations             :: Word16
      , dh_header_paras_size       :: Word16
      , dh_min_extra_paras         :: Word16
      , dh_max_extra_paras         :: Word16
      , dh_initial_relative_ss     :: Word16
      , dh_initial_sp              :: Word16
      , dh_header_checksum         :: Word16
      , dh_initial_ip              :: Word16   
      , dh_initial_relative_cs     :: Word16
      , dh_reltable_file_addr      :: Word16
      , dh_overlay_number          :: Word16
      , dh_reserved_words          :: (Word16,Word16,Word16,Word16)
      , dh_oem_identifier          :: Word16
      , dh_oem_info                :: Word16
      , dh_reserved_words_two      :: [Word16]   -- length 10
      , dh_new_exe_addr            :: Word32
      }  
  deriving Show



image_COFF_HEADER_size :: Int
image_COFF_HEADER_size = 20

data COFFHeader = COFFHeader 
      { ch_machine                  :: Word16
      , ch_num_sections             :: Word16
      , ch_timedate_stamp           :: Word32
      , ch_sym_table_ptr            :: Word32
      , ch_num_symbols              :: Word32
      , ch_opt_header_size          :: Word16
      , ch_characteristics          :: Word16
      }
   deriving Show


data ImageOptionalHeader = ImageOptionalHeader
      { ioh_header_std_fields       :: OptionalStandardHeader
      , ioh_nt_specific_fields      :: OptionalWindowsHeader
      , ioh_data_directory          :: [HeaderDataDirectory]
      }
  deriving Show
 

data OptionalStandardHeader = OptionalStandardHeader
      { osh_magic                   :: Word16
      , osh_major_linker_version    :: Word8
      , osh_minor_linker_version    :: Word8
      , osh_size_of_code            :: Word32
      , osh_size_of_inited_data     :: Word32
      , osh_size_of_uninited_data   :: Word32
      , osh_entry_point_addr        :: Word32
      , osh_base_of_code            :: Word32
      , osh_base_of_data            :: Word32
      }
   deriving Show

data OptionalWindowsHeader = OptionalWindowsHeader
      { owh_image_base              :: Word32
      , owh_section_alignment       :: Word32
      , owh_file_alignment          :: Word32
      , owh_major_os_version        :: Word16
      , owh_minor_os_version        :: Word16
      , owh_major_image_version     :: Word16
      , owh_minor_image_version     :: Word16
      , owh_major_subsys_version    :: Word16
      , owh_minor_subsys_version    :: Word16
      , owh_win32_version           :: Word32
      , owh_size_of_image           :: Word32
      , owh_size_of_headers         :: Word32
      , owh_checksum                :: Word32
      , owh_subsystem               :: Word16
      , owh_dll_characteristics     :: Word16
      , owh_size_stack_reserve      :: Word32
      , owh_size_stack_commit       :: Word32
      , owh_size_heap_reserve       :: Word32
      , owh_size_heap_commit        :: Word32
      , owh_loader_flags            :: Word32
      , owh_rva_num_and_sizes       :: Word32
      } 
  deriving Show

image_DATA_DIRECTORY_size :: Int
image_DATA_DIRECTORY_size = 4

data HeaderDataDirectory = HeaderDataDirectory
      { hdd_virtual_addr            :: Word32
      , hdd_size                    :: Word32
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





