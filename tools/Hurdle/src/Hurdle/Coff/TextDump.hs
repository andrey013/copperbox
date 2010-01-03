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
-- Pretty print as text...
--
--------------------------------------------------------------------------------

module Hurdle.Coff.TextDump where

import Hurdle.Base.Utils ( applyfs )
import Hurdle.Coff.Datatypes

import qualified Data.Map as Map
import Data.Word
import Numeric
import Text.PrettyPrint.JoinPrint



printImage :: Image -> IO ()
printImage = putStr . imageText

imageText :: Image -> String
imageText = render . ppImage

printCOFF :: COFFHeader -> IO ()
printCOFF = putStr . coff

coff :: COFFHeader -> String
coff = render . ppCOFFHeader

ppImage :: Image -> Doc
ppImage a = 
        ppDOSHeader           (image_dos_header a)
    <%> columnSep
    <%> ppSignature           (image_signature a)
    <%> ppCOFFHeader          (image_coff_header a)
    <%> ppImageOptionalHeader (image_opt_header a)
    <%> (vcat $ map ppSectionHeader $ Map.elems $ image_section_headers a)
    <%> maybe empty ppExportData          (image_export_data a)


ppDOSHeader :: DOSHeader -> Doc
ppDOSHeader a = 
    tableProlog "IMAGE_DOS_HEADER" (24,6) (applyfs fields a) 
  where
    ppf    = ppField 4 24   
    fields = 
       [ ppf 2  "magic"                 (ppHex 4 . dh_magic_number)
       , ppf 2  "bytes last page"       (ppHex 4 . dh_bytes_on_last_page)
       , ppf 2  "pages in file"         (ppHex 4 . dh_pages_in_file)
       , ppf 2  "relocations"           (ppHex 4 . dh_relocations)
       , ppf 2  "header para size"      (ppHex 4 . dh_header_paras_size)
       , ppf 2  "min extra paragraphs"  (ppHex 4 . dh_min_extra_paras)
       , ppf 2  "max extra paragraphs"  (ppHex 4 . dh_max_extra_paras)
       , ppf 2  "initial SS value"      (ppHex 4 . dh_initial_relative_ss)
       , ppf 2  "initial SP value"      (ppHex 4 . dh_initial_sp)
       , ppf 2  "checksum"              (ppHex 4 . dh_header_checksum)

       , ppf 2  "initial IP value"      (ppHex 4 . dh_initial_ip)
       , ppf 2  "initial CS value"      (ppHex 4 . dh_initial_relative_cs)
       , ppf 2  "relocation table addr" (ppHex 4 . dh_reltable_file_addr)
       , ppf 2  "overlay number"        (ppHex 4 . dh_overlay_number)
       , ppf 8  "reserved 1"            (tup4    . dh_reserved_words)
       , ppf 2  "oem identifier"        (ppHex 4 . dh_oem_identifier)
       , ppf 2  "oem info"              (ppHex 4 . dh_oem_info)
       , ppf 20 "reserved 2"            (text . show . dh_reserved_words_two)
       , ppf 4  "new exe header addr"   (ppHex 8 . dh_new_exe_addr)
       ]

    tup4 (s,t,u,v) = text $ show [s,t,u,v]

ppSignature :: (Char,Char,Char,Char) -> Doc
ppSignature (s,t,u,v) = 
    text "Signature:" <+> (listDoc $ map (text . show) [s,t,u,v])

ppCOFFHeader :: COFFHeader -> Doc
ppCOFFHeader a = 
    tableProlog "COFF HEADER" (24,6) (applyfs fields a) 
  where
    ppf    = ppField 4 24   
    fields = 
       [ ppf 2  "machine"               (ppHex 4 . ch_machine)
       , ppf 2  "num sections"          (ppHex 4 . ch_num_sections)
       , ppf 4  "timedatestamp"         (ppHex 8 . ch_timedate_stamp)
       , ppf 4  "ptr to sym table"      (ppHex 8 . ch_sym_table_ptr)
       , ppf 4  "num symbols"           (ppHex 8 . ch_num_symbols)
       , ppf 2  "size optional header"  (ppHex 4 . ch_opt_header_size)
       , ppf 2  "characteristics"       (ppHex 4 . ch_characteristics)
       ]


ppImageOptionalHeader :: ImageOptionalHeader -> Doc
ppImageOptionalHeader a = 
        ppOptionalStandardHeader   (ioh_header_std_fields a)
    <%> ppOptionalWindowsHeader   (ioh_nt_specific_fields a)
    <%> (vcat $ zipWith ppHeaderDataDirectory names (ioh_data_directory a))
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

ppOptionalStandardHeader :: OptionalStandardHeader -> Doc
ppOptionalStandardHeader a =
    tableProlog "IMAGE OPTIONAL HEADER STANDARD" (24,6) (applyfs fields a) 
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 2  "magic"                 (ppHex 4 . osh_magic)
       , ppf 1  "major linker ver."     (ppHex 2 . osh_major_linker_version)
       , ppf 1  "minor linker ver."     (ppHex 2 . osh_minor_linker_version)
       , ppf 4  "size of code"          (ppHex 8 . osh_size_of_code)
       , ppf 4  "size of init. data"    (ppHex 8 . osh_size_of_inited_data)
       , ppf 4  "size of uninit. data"  (ppHex 8 . osh_size_of_uninited_data)
       , ppf 4  "entry ptr addr"        (ppHex 8 . osh_entry_point_addr)
       , ppf 4  "base of code"          (ppHex 8 . osh_base_of_code)
       , ppf 4  "base of data"          (ppHex 8 . osh_base_of_data)
       ]


ppOptionalWindowsHeader :: OptionalWindowsHeader -> Doc
ppOptionalWindowsHeader a =
    tableProlog "IMAGE OPTIONAL HEADER NT SPECIFIC" (24,6) (applyfs fields a)
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 4  "image base"            (ppHex 8 . owh_image_base)
       , ppf 4  "section alignment"     (ppHex 8 . owh_section_alignment)
       , ppf 4  "file alignment"        (ppHex 8 . owh_file_alignment)
       , ppf 2  "major os version"      (ppHex 4 . owh_major_os_version)
       , ppf 2  "minor os version"      (ppHex 4 . owh_minor_os_version)
       , ppf 2  "major image version"   (ppHex 4 . owh_major_image_version)
       , ppf 2  "minor image version"   (ppHex 4 . owh_minor_image_version)
       , ppf 2  "major subsys version"  (ppHex 4 . owh_major_subsys_version)
       , ppf 2  "minor subsys version"  (ppHex 4 . owh_minor_subsys_version)
       , ppf 4  "win32 version"         (ppHex 8 . owh_win32_version)
       , ppf 4  "size of image"         (ppHex 8 . owh_size_of_image)
       , ppf 4  "size of headers"       (ppHex 8 . owh_size_of_headers)
       , ppf 4  "checksum"              (ppHex 8 . owh_checksum)
       , ppf 2  "subsystem"             (ppHex 4 . owh_subsystem)
       , ppf 2  "dll characteristics"   (ppHex 4 . owh_dll_characteristics)
       , ppf 4  "size of stack reserve" (ppHex 8 . owh_size_stack_reserve)
       , ppf 4  "size of stack commit"  (ppHex 8 . owh_size_stack_commit)
       , ppf 4  "size of heap reserve"  (ppHex 8 . owh_size_heap_reserve)
       , ppf 4  "size of heap commit"   (ppHex 8 . owh_size_heap_commit)
       , ppf 4  "loader flags"          (ppHex 8 . owh_loader_flags)
       , ppf 4  "rva num and sizes"     (ppHex 8 . owh_rva_num_and_sizes)
       ]

ppHeaderDataDirectory :: String -> HeaderDataDirectory -> Doc
ppHeaderDataDirectory s a =
    tableProlog s (24,6) (applyfs fields a) 
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 4  "virtual address"       (ppHex 8 . hdd_virtual_addr)
       , ppf 4  "size"                  (ppHex 8 . hdd_size)
       ]


ppSectionHeader :: SectionHeader -> Doc
ppSectionHeader a = 
    tableProlog "SECTION HEADER" (24,6) (applyfs fields a)
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 8  "name"                  (text    . sh_name)
       , ppf 4  "virtual size"          (ppHex 8 . sh_virtual_size)
       , ppf 4  "virtual addr"          (ppHex 8 . sh_virtual_addr)
       , ppf 4  "size of raw data"      (ppHex 8 . sh_size_raw_data)
       , ppf 4  "ptr to raw data"       (ppHex 8 . sh_ptr_raw_data)
       , ppf 4  "ptr to relocations"    (ppHex 8 . sh_ptr_relocations)
       , ppf 4  "ptr to line numbers"   (ppHex 8 . sh_ptr_linenums)
       , ppf 2  "num of relocations"    (ppHex 4 . sh_num_relocations)
       , ppf 2  "num of line numbers"   (ppHex 4 . sh_num_linenums)
       , ppf 4  "characteristics"       (ppHex 8 . sh_characteristics)
       ]

ppExportData :: ExportData -> Doc
ppExportData a = 
        ppExportDirectoryTable (ed_directory_table a)
    <%> ppExportAddressTable   (ed_export_address_table a)
    <%> ppExportNamePtrTable   (ed_name_ptr_table a)
    <%> ppExportOrdinalTable   (ed_ordinal_table a)
    <%> ppExportNames          (ed_name_table a)

ppExportDirectoryTable :: ExportDirectoryTable -> Doc
ppExportDirectoryTable a = 
    tableProlog ".edata (Export directory table)" (24,6) (applyfs fields a)
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 8  "export flags"          (ppHex 8 . edt_export_flags)
       , ppf 8  "timedate stamp"        (ppHex 8 . edt_timedate_stamp)
       , ppf 4  "major version"         (ppHex 4 . edt_major_version)
       , ppf 4  "minor version"         (ppHex 4 . edt_minor_version)
       , ppf 8  "name rva"              (ppHex 8 . edt_name_rva)
       , ppf 8  "oridinal base"         (ppHex 8 . edt_ordinal_base)
       , ppf 8  "addr table entries"    (ppHex 8 . edt_num_addr_table_entries)
       , ppf 8  "num name ptrs"         (ppHex 8 . edt_num_name_ptrs)
       , ppf 8  "export addr table rva" (ppHex 8 . edt_export_addr_table_rva)
       , ppf 8  "name ptr rva"          (ppHex 8 . edt_name_ptr_table_rva)
       , ppf 8  "ordinal table rva"     (ppHex 8 . edt_ordinal_table_rva)
       ]

ppExportAddressTable :: ExportAddressTable -> Doc
ppExportAddressTable a = 
    tableProlog "Export address" (24,6) (map field $ getExportAddressTable a)
  where
    ppf    = ppField 4 24
    field (EA_Export_RVA w32)    = ppf 8 "export RVA"    (ppHex 8) w32
    field (EA_Forwarder_RVA w32) = ppf 8 "forwarder RVA" (ppHex 8) w32
      

ppExportNamePtrTable :: [Word32] -> Doc
ppExportNamePtrTable a = 
    tableProlog "Export name pointers" (24,6) (map field a)
  where
    ppf    = ppField 4 24
    field  = ppf 8 "name ptr"  (ppHex 8)

ppExportOrdinalTable :: [Word16] -> Doc
ppExportOrdinalTable a = 
    tableProlog "Export ordinals" (24,6) (map field a)
  where
    ppf    = ppField 4 24
    field  = ppf 4 "ordinal"    (ppHex 4) 

ppExportNames :: [String] -> Doc
ppExportNames a = 
    tableProlog "Export names" (24,6) (map field a)
  where
    ppf    = ppField 4 24
    field  = ppf 4 "export name" text 



--------------------------------------------------------------------------------
-- Helpers

tableProlog :: String -> (Int,Int) -> [Doc] -> Doc
tableProlog s (m,n) ds =
        columnSep 
    <%> text s 
    <%> columnSep
    <%> columnHeadings m n
    <%> columnSep
    <%> vcat ds
  where
    columnHeadings fsz vsz = 
      text "size" <+> text (pad fsz ' ' "field") <+> text (pad vsz ' ' "value")


columnSep :: Doc
columnSep = text $ replicate 60 '-' 


ppField :: Int -> Int -> Int -> String -> (a -> Doc) -> a -> Doc
ppField n1 n2 sz field_name f a = text sz'  <+> text field_name' <+> f a
  where
    sz'         = pad n1 ' ' (show sz)
    field_name' = pad n2 ' ' field_name

ppHex :: Integral a => Int -> a -> Doc
ppHex n i = text "0x" <> (text $ pad n '0' $ showHex i "")
  

pad :: Int -> Char -> String -> String
pad n ch s | length s < n = replicate (n - length s) ch ++ s
           | otherwise    = s

listDoc :: [Doc] -> Doc
listDoc = brackets . punctuate comma

