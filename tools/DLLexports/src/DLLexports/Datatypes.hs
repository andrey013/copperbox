{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DLLexports.Datatypes
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

module DLLexports.Datatypes where

import Data.Word

data Image = Image 
      { image_dos_header            :: ImageDOSHeader
      , image_signature             :: (Char,Char,Char,Char)
      , image_coff_header           :: ImageCOFFHeader
      , image_opt_header            :: ImageOptionalHeader
      }
  deriving Show

image_DOS_HEADER_size :: Int
image_DOS_HEADER_size = 64

data ImageDOSHeader = ImageDOSHeader 
      { dos_magic_number            :: Word16
      , dos_bytes_last_page         :: Word16
      , dos_pages_in_file           :: Word16
      , dos_relocations             :: Word16
      , dos_size_header_paras       :: Word16
      , dos_min_extra_paras         :: Word16
      , dos_max_extra_paras         :: Word16
      , dos_initial_relative_ss     :: Word16
      , dos_initial_sp              :: Word16
      , dos_header_checksum         :: Word16
      , dos_initial_ip              :: Word16   
      , dos_initial_relative_cs     :: Word16
      , dos_reltable_file_addr      :: Word16
      , dos_overlay_number          :: Word16
      , dos_reserved_words          :: (Word16,Word16,Word16,Word16)
      , dos_oem_identifier          :: Word16
      , dos_oem_info                :: Word16
      , dos_reserved_words_two      :: [Word16]   -- length 10
      , dos_new_exe_header_addr     :: Word32
      }  
  deriving Show



image_COFF_HEADER_size :: Int
image_COFF_HEADER_size = 20

data ImageCOFFHeader = ImageCOFFHeader 
      { imgf_machine               :: Word16
      , imgf_num_sections          :: Word16
      , imgf_timedate_stamp        :: Word32
      , imgf_sym_table_ptr         :: Word32
      , imgf_num_symbols           :: Word32
      , imgf_opt_header_size       :: Word16
      , imgf_characteristics       :: Word16
      }
   deriving Show

image_OPTIONAL_HEADER_size :: Int
image_OPTIONAL_HEADER_size = 
    image_OPTIONAL_STANDARD_size + image_OPTIONAL_NT_SPECIFIC_size
       + 16*image_DATA_DIRECTORY_size

data ImageOptionalHeader = ImageOptionalHeader
      { iopt_header_std_fields      :: ImageOptionalStandard
      , iopt_nt_specific_fields     :: ImageOptionalNTSpecific
      , iopt_data_directory         :: [ImageDataDirectory]
      }
  deriving Show
 

image_OPTIONAL_STANDARD_size :: Int
image_OPTIONAL_STANDARD_size = 28

data ImageOptionalStandard = ImageOptionalStandard
      { iopt_magic                  :: Word16
      , iopt_major_linker_version   :: Word8
      , iopt_minor_linker_version   :: Word8
      , iopt_size_of_code           :: Word32
      , iopt_size_of_inited_data    :: Word32
      , iopt_size_of_uninited_data  :: Word32
      , iopt_entry_point_addr       :: Word32
      , iopt_base_of_code           :: Word32
      , iopt_base_of_data           :: Word32
      }
   deriving Show

image_OPTIONAL_NT_SPECIFIC_size :: Int
image_OPTIONAL_NT_SPECIFIC_size = 68

data ImageOptionalNTSpecific = ImageOptionalNTSpecific
      { iopy_image_base             :: Word32
      , iopt_section_alignment      :: Word32
      , iopt_file_alignment         :: Word32
      , iopt_major_os_version       :: Word16
      , iopt_minor_os_version       :: Word16
      , iopt_major_image_version    :: Word16
      , iopt_minor_image_version    :: Word16
      , iopt_major_subsys_version   :: Word16
      , iopt_minor_subsys_version   :: Word16
      , iopt_win32_version          :: Word32
      , iopt_size_of_image          :: Word32
      , iopt_size_of_headers        :: Word32
      , iopt_checksum               :: Word32
      , iopt_subsystem              :: Word16
      , iopt_dll_characteristics    :: Word16
      , iopt_size_stack_reserve     :: Word32
      , iopt_size_stack_commit      :: Word32
      , iopt_size_heap_reserve      :: Word32
      , iopt_size_heap_commit       :: Word32
      , iopt_loader_flags           :: Word32
      , iopt_rva_num_and_sizes      :: Word32
      } 
  deriving Show

image_DATA_DIRECTORY_size :: Int
image_DATA_DIRECTORY_size = 4

data ImageDataDirectory = ImageDataDirectory
      { dd_virtual_addr             :: Word32
      , dd_size                     :: Word32
      }
  deriving Show

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