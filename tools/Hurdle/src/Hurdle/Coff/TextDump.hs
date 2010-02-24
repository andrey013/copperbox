{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Coff.TextDump
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  
--
-- Pretty print as text... This is horrible and needs rethinking...
--
--------------------------------------------------------------------------------

module Hurdle.Coff.TextDump where

import Hurdle.Base.Table
import Hurdle.Coff.Datatypes

import qualified Data.Map as Map
import Data.Word
import Text.PrettyPrint.JoinPrint hiding ( length )



printImage :: Image -> IO ()
printImage = putStr . imageText

imageText :: Image -> String
imageText = show . ppImage

printCOFF :: COFFHeader -> IO ()
printCOFF = putStr . coff

coff :: COFFHeader -> String
coff = show . ppCOFFHeader

ppImage :: Image -> VDoc
ppImage a = vconcat $
    [ ppDOSHeader           (image_dos_header a)
    , rowSep
    , vdoc $ ppSignature    (image_signature a)
    , ppCOFFHeader          (image_coff_header a)
    , ppImageOptionalHeader (image_opt_header a)
    , (vconcat $ map ppSectionHeader $ Map.elems $ image_section_headers a)
    , maybe (vdoc empty) ppExportData          (image_export_data a)
    ]

ppDOSHeader :: DOSHeader -> VDoc
ppDOSHeader = mkTable "IMAGE_DOS_HEADER" . vcat . sequence fields
  where
    fields = 
      [ recRow 2  "magic"                 . oxhex4 . dh_magic_number
      , recRow 2  "bytes last page"       . oxhex4 . dh_bytes_on_last_page
      , recRow 2  "pages in file"         . oxhex4 . dh_pages_in_file
      , recRow 2  "relocations"           . oxhex4 . dh_relocations
      , recRow 2  "header para size"      . oxhex4 . dh_header_paras_size
      , recRow 2  "min extra paragraphs"  . oxhex4 . dh_min_extra_paras
      , recRow 2  "max extra paragraphs"  . oxhex4 . dh_max_extra_paras
      , recRow 2  "initial SS value"      . oxhex4 . dh_initial_relative_ss
      , recRow 2  "initial SP value"      . oxhex4 . dh_initial_sp
      , recRow 2  "checksum"              . oxhex4 . dh_header_checksum

      , recRow 2  "initial IP value"      . oxhex4 . dh_initial_ip
      , recRow 2  "initial CS value"      . oxhex4 . dh_initial_relative_cs
      , recRow 2  "relocation table addr" . oxhex4 . dh_reltable_file_addr
      , recRow 2  "overlay number"        . oxhex4 . dh_overlay_number
      , recRow 8  "reserved 1"            . tup4   . dh_reserved_words
      , recRow 2  "oem identifier"        . oxhex4 . dh_oem_identifier
      , recRow 2  "oem info"              . oxhex4 . dh_oem_info
      , recRow 20 "reserved 2"            . text . show . dh_reserved_words_two
      , recRow 4  "new exe header addr"   . oxhex8 . dh_new_exe_addr
      ]

    tup4 (s,t,u,v) = text $ show [s,t,u,v]

ppSignature :: (Char,Char,Char,Char) -> Doc
ppSignature (s,t,u,v) = 
    text "Signature:" <+> (list $ map (text . show) [s,t,u,v])

ppCOFFHeader :: COFFHeader -> VDoc
ppCOFFHeader = mkTable "COFF HEADER"  . vcat . sequence fields
  where
    fields = 
       [ recRow 2  "machine"               . oxhex4 . ch_machine
       , recRow 2  "num sections"          . oxhex4 . ch_num_sections
       , recRow 4  "timedatestamp"         . oxhex8 . ch_timedate_stamp
       , recRow 4  "ptr to sym table"      . oxhex8 . ch_sym_table_ptr
       , recRow 4  "num symbols"           . oxhex8 . ch_num_symbols
       , recRow 2  "size optional header"  . oxhex4 . ch_opt_header_size
       , recRow 2  "characteristics"       . oxhex4 . ch_characteristics
       ]


ppImageOptionalHeader :: ImageOptionalHeader -> VDoc
ppImageOptionalHeader a = vconcat $
    [ ppOptionalStandardHeader   $ ioh_header_std_fields a
    , ppOptionalWindowsHeader    $ ioh_nt_specific_fields a
    , vconcat $ zipWith ppHeaderDataDirectory names $ ioh_data_directory a
    ]   
  where
    names = [ ".edata"
            , ".idata"
            , ".rsrc"
            , ".pdata"
            , "attribute certificate table"
            , ".reloc"
            , ".debug"
            , "architecture"
            , "global ptr"
            , ".tls"
            , "load config"
            , "bound impact"
            , "import address table"
            , "delay import descriptor"
            , ".cormeta"
            , "reserved"
            ]

ppOptionalStandardHeader :: OptionalStandardHeader -> VDoc
ppOptionalStandardHeader = 
    mkTable "IMAGE OPTIONAL HEADER STANDARD" . vcat . sequence fields
  where
    fields = 
       [ recRow 2  "magic"                 . oxhex4 . osh_magic
       , recRow 1  "major linker ver."     . oxhex2 . osh_major_linker_version
       , recRow 1  "minor linker ver."     . oxhex2 . osh_minor_linker_version
       , recRow 4  "size of code"          . oxhex8 . osh_size_of_code
       , recRow 4  "size of init. data"    . oxhex8 . osh_size_of_inited_data
       , recRow 4  "size of uninit. data"  . oxhex8 . osh_size_of_uninited_data
       , recRow 4  "entry ptr addr"        . oxhex8 . osh_entry_point_addr
       , recRow 4  "base of code"          . oxhex8 . osh_base_of_code
       , recRow 4  "base of data"          . oxhex8 . osh_base_of_data
       ]


ppOptionalWindowsHeader :: OptionalWindowsHeader -> VDoc
ppOptionalWindowsHeader =
    mkTable "IMAGE OPTIONAL HEADER NT SPECIFIC" . vcat . sequence fields
  where
    fields = 
       [ recRow 4  "image base"            . oxhex8 . owh_image_base
       , recRow 4  "section alignment"     . oxhex8 . owh_section_alignment
       , recRow 4  "file alignment"        . oxhex8 . owh_file_alignment
       , recRow 2  "major os version"      . oxhex4 . owh_major_os_version
       , recRow 2  "minor os version"      . oxhex4 . owh_minor_os_version
       , recRow 2  "major image version"   . oxhex4 . owh_major_image_version
       , recRow 2  "minor image version"   . oxhex4 . owh_minor_image_version
       , recRow 2  "major subsys version"  . oxhex4 . owh_major_subsys_version
       , recRow 2  "minor subsys version"  . oxhex4 . owh_minor_subsys_version
       , recRow 4  "win32 version"         . oxhex8 . owh_win32_version
       , recRow 4  "size of image"         . oxhex8 . owh_size_of_image
       , recRow 4  "size of headers"       . oxhex8 . owh_size_of_headers
       , recRow 4  "checksum"              . oxhex8 . owh_checksum
       , recRow 2  "subsystem"             . oxhex4 . owh_subsystem
       , recRow 2  "dll characteristics"   . oxhex4 . owh_dll_characteristics
       , recRow 4  "size of stack reserve" . oxhex8 . owh_size_stack_reserve
       , recRow 4  "size of stack commit"  . oxhex8 . owh_size_stack_commit
       , recRow 4  "size of heap reserve"  . oxhex8 . owh_size_heap_reserve
       , recRow 4  "size of heap commit"   . oxhex8 . owh_size_heap_commit
       , recRow 4  "loader flags"          . oxhex8 . owh_loader_flags
       , recRow 4  "rva num and sizes"     . oxhex8 . owh_rva_num_and_sizes
       ]

ppHeaderDataDirectory :: String -> HeaderDataDirectory -> VDoc
ppHeaderDataDirectory name = mkTable name . vcat . sequence fields
  where
    fields = 
      [ recRow 4  "virtual address"       . oxhex8 . hdd_virtual_addr
      , recRow 4  "size"                  . oxhex8 . hdd_size
      ]


ppSectionHeader :: SectionHeader -> VDoc
ppSectionHeader = mkTable "SECTION HEADER" . vcat . sequence fields
  where
    fields = 
      [ recRow 8  "name"                  . text   . sh_name
      , recRow 4  "virtual size"          . oxhex8 . sh_virtual_size
      , recRow 4  "virtual addr"          . oxhex8 . sh_virtual_addr
      , recRow 4  "size of raw data"      . oxhex8 . sh_size_raw_data
      , recRow 4  "ptr to raw data"       . oxhex8 . sh_ptr_raw_data
      , recRow 4  "ptr to relocations"    . oxhex8 . sh_ptr_relocations
      , recRow 4  "ptr to line numbers"   . oxhex8 . sh_ptr_linenums
      , recRow 2  "num of relocations"    . oxhex4 . sh_num_relocations
      , recRow 2  "num of line numbers"   . oxhex4 . sh_num_linenums
      , recRow 4  "characteristics"       . oxhex8 . sh_characteristics
      ]

ppExportData :: ExportData -> VDoc
ppExportData = vconcat . sequence tables
  where
    tables = 
      [ ppExportDirectoryTable . ed_directory_table
      , ppExportAddressTable   . ed_export_address_table
      , ppExportNamePtrTable   . ed_name_ptr_table
      , ppExportOrdinalTable   . ed_ordinal_table
      , ppExportNames          . ed_name_table
      ]

ppExportDirectoryTable :: ExportDirectoryTable -> VDoc
ppExportDirectoryTable = 
    mkTable ".edata (Export directory table)" . vcat . sequence fields
  where
    fields = 
      [ recRow 8 "export flags"           . oxhex8 . edt_export_flags
      , recRow 8  "timedate stamp"        . oxhex8 . edt_timedate_stamp
      , recRow 4  "major version"         . oxhex4 . edt_major_version
      , recRow 4  "minor version"         . oxhex4 . edt_minor_version
      , recRow 8  "name rva"              . oxhex8 . edt_name_rva
      , recRow 8  "oridinal base"         . oxhex8 . edt_ordinal_base
      , recRow 8  "addr table entries"    . oxhex8 . edt_num_addr_table_entries
      , recRow 8  "num name ptrs"         . oxhex8 . edt_num_name_ptrs
      , recRow 8  "export addr table rva" . oxhex8 . edt_export_addr_table_rva
      , recRow 8  "name ptr rva"          . oxhex8 . edt_name_ptr_table_rva
      , recRow 8  "ordinal table rva"     . oxhex8 . edt_ordinal_table_rva
      ]



ppExportAddressTable :: ExportAddressTable -> VDoc
ppExportAddressTable = 
    mkTable "Export address" . vcat . map mkRow . getExportAddressTable
  where
    mkRow (EA_Export_RVA    n) = row3 (int 8) (text "export RVA")    (oxhex8 n)
    mkRow (EA_Forwarder_RVA n) = row3 (int 8) (text "forwarder RVA") (oxhex8 n)
      

ppExportNamePtrTable :: [Word32] -> VDoc
ppExportNamePtrTable xs = mkTable "Export name pointers" $ vcat $ map mkRow xs
  where
    mkRow  = row3 (int 8) (text "name ptr") . oxhex8

ppExportOrdinalTable :: [Word16] -> VDoc
ppExportOrdinalTable xs = mkTable "Export ordinals"  $ vcat $ map mkRow xs
  where
    mkRow = row3 (int 4) (text "ordinal") . oxhex4

ppExportNames :: [String] -> VDoc
ppExportNames xs = mkTable "Export names" $ vcat $ map mkRow xs
  where
    mkRow = row3 (text "-") (text "export name") . text



--------------------------------------------------------------------------------
-- Helpers

-- 4r-24r-24l

colHeadings :: VDoc 
colHeadings = vdoc $ row3 (text "size") (text "field") (text "value") 

row3 :: Doc -> Doc -> Doc -> Doc
row3 = row dblspace (alignRight 4 $ alignRight 24 $ alignLeft 24 $ endrow) 

recRow :: Int -> String -> Doc -> Doc
recRow fsize name doc = row3 (int fsize) (text name) doc

type TableName = String

mkTable :: TableName -> VDoc -> VDoc
mkTable tname rows = title `vcons` (block1 `vappend` rows)
  where
    title  = text tname
    block1 = vconcat [rowSep, colHeadings, rowSep] 


rowSep :: VDoc
rowSep = vdoc $ replicateChar 60 '-' 


-- TO ADD TO JoinPrint
list :: [Doc] -> Doc
list = brackets . punctuate comma

vdoc :: Doc -> VDoc
vdoc = vcat . return

vappend :: VDoc -> VDoc -> VDoc
vappend a b = vconcat [a,b]
