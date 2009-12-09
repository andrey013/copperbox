{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DLLexports.TextDump
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print as text...
--
--------------------------------------------------------------------------------

module DLLexports.TextDump where

import DLLexports.Datatypes

import Data.Word
import Numeric
import Text.PrettyPrint.HughesPJ


printImage :: Image -> IO ()
printImage = putStr . imageText

imageText :: Image -> String
imageText = renderStyle (Style PageMode 80 1.5) . ppImage

ppImage :: Image -> Doc
ppImage a = 
        ppImageDOSHeader      (image_dos_header a)
    $+$ columnSep
    $+$ ppSignature           (image_signature a)
    $+$ ppImageCOFFHeader     (image_coff_header a)
    $+$ ppImageOptionalHeader (image_opt_header a)
    $+$ (vcat $ map ppSectionHeader $ image_section_headers a)
    $+$ ppExportData          (image_export_data a)


ppImageDOSHeader :: ImageDOSHeader -> Doc
ppImageDOSHeader a = 
    tableProlog "IMAGE_DOS_HEADER" (24,6) (sequence fields a) 
  where
    ppf    = ppField 4 24   
    fields = 
       [ ppf 2  "magic"                 (ppHex 4 . idh_magic_number)
       , ppf 2  "bytes last page"       (ppHex 4 . idh_bytes_last_page)
       , ppf 2  "pages in file"         (ppHex 4 . idh_pages_in_file)
       , ppf 2  "relocations"           (ppHex 4 . idh_relocations)
       , ppf 2  "header para size"      (ppHex 4 . idh_size_header_paras)
       , ppf 2  "min extra paragraphs"  (ppHex 4 . idh_min_extra_paras)
       , ppf 2  "max extra paragraphs"  (ppHex 4 . idh_max_extra_paras)
       , ppf 2  "initial SS value"      (ppHex 4 . idh_initial_relative_ss)
       , ppf 2  "initial SP value"      (ppHex 4 . idh_initial_sp)
       , ppf 2  "checksum"              (ppHex 4 . idh_header_checksum)

       , ppf 2  "initial IP value"      (ppHex 4 . idh_initial_ip)
       , ppf 2  "initial CS value"      (ppHex 4 . idh_initial_relative_cs)
       , ppf 2  "relocation table addr" (ppHex 4 . idh_reltable_file_addr)
       , ppf 2  "overlay number"        (ppHex 4 . idh_overlay_number)
       , ppf 8 "reserved 1"             (tup4    . idh_reserved_words)
       , ppf 2  "oem identifier"        (ppHex 4 . idh_oem_identifier)
       , ppf 2  "oem info"              (ppHex 4 . idh_oem_info)
       , ppf 20 "reserved 2"            (text . show . idh_reserved_words_two)
       , ppf 4  "new exe header addr"   (ppHex 8 . idh_new_exe_header_addr)
       ]

    tup4 (s,t,u,v) = text $ show [s,t,u,v]

ppSignature :: (Char,Char,Char,Char) -> Doc
ppSignature (s,t,u,v) = 
    text "Signature:" <+> (listDoc $ map (text . show) [s,t,u,v])

ppImageCOFFHeader :: ImageCOFFHeader -> Doc
ppImageCOFFHeader a = 
    tableProlog "IMAGE COFF HEADER" (24,6) (sequence fields a) 
  where
    ppf    = ppField 4 24   
    fields = 
       [ ppf 2  "machine"               (ppHex 4 . ich_machine)
       , ppf 2  "num sections"          (ppHex 4 . ich_num_sections)
       , ppf 4  "timedatestamp"         (ppHex 8 . ich_timedate_stamp)
       , ppf 4  "ptr to sym table"      (ppHex 8 . ich_sym_table_ptr)
       , ppf 4  "num symbols"           (ppHex 8 . ich_num_symbols)
       , ppf 2  "size optional header"  (ppHex 4 . ich_opt_header_size)
       , ppf 2  "characteristics"       (ppHex 4 . ich_characteristics)
       ]


ppImageOptionalHeader :: ImageOptionalHeader -> Doc
ppImageOptionalHeader a = 
        ppImageOptionalStandard   (ioh_header_std_fields a)
    $+$ ppImageOptionalNTSpecific (ioh_nt_specific_fields a)
    $+$ (vcat $ zipWith ppImageDataDirectory names (ioh_data_directory a))
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

ppImageOptionalStandard :: ImageOptionalStandard -> Doc
ppImageOptionalStandard a =
    tableProlog "IMAGE OPTIONAL HEADER STANDARD" (24,6) (sequence fields a) 
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 2  "magic"                 (ppHex 4 . ios_magic)
       , ppf 1  "major linker ver."     (ppHex 2 . ios_major_linker_version)
       , ppf 1  "minor linker ver."     (ppHex 2 . ios_minor_linker_version)
       , ppf 4  "size of code"          (ppHex 8 . ios_size_of_code)
       , ppf 4  "size of init. data"    (ppHex 8 . ios_size_of_inited_data)
       , ppf 4  "size of uninit. data"  (ppHex 8 . ios_size_of_uninited_data)
       , ppf 4  "entry ptr addr"        (ppHex 8 . ios_entry_point_addr)
       , ppf 4  "base of code"          (ppHex 8 . ios_base_of_code)
       , ppf 4  "base of data"          (ppHex 8 . ios_base_of_data)
       ]


ppImageOptionalNTSpecific :: ImageOptionalNTSpecific -> Doc
ppImageOptionalNTSpecific a =
    tableProlog "IMAGE OPTIONAL HEADER NT SPECIFIC" (24,6) (sequence fields a)
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 4  "image base"            (ppHex 8 . iont_image_base)
       , ppf 4  "section alignment"     (ppHex 8 . iont_section_alignment)
       , ppf 4  "file alignment"        (ppHex 8 . iont_file_alignment)
       , ppf 2  "major os version"      (ppHex 4 . iont_major_os_version)
       , ppf 2  "minor os version"      (ppHex 4 . iont_minor_os_version)
       , ppf 2  "major image version"   (ppHex 4 . iont_major_image_version)
       , ppf 2  "minor image version"   (ppHex 4 . iont_minor_image_version)
       , ppf 2  "major subsys version"  (ppHex 4 . iont_major_subsys_version)
       , ppf 2  "minor subsys version"  (ppHex 4 . iont_minor_subsys_version)
       , ppf 4  "win32 version"         (ppHex 8 . iont_win32_version)
       , ppf 4  "size of image"         (ppHex 8 . iont_size_of_image)
       , ppf 4  "size of headers"       (ppHex 8 . iont_size_of_headers)
       , ppf 4  "checksum"              (ppHex 8 . iont_checksum)
       , ppf 2  "subsystem"             (ppHex 4 . iont_subsystem)
       , ppf 2  "dll characteristics"   (ppHex 4 . iont_dll_characteristics)
       , ppf 4  "size of stack reserve" (ppHex 8 . iont_size_stack_reserve)
       , ppf 4  "size of stack commit"  (ppHex 8 . iont_size_stack_commit)
       , ppf 4  "size of heap reserve"  (ppHex 8 . iont_size_heap_reserve)
       , ppf 4  "size of heap commit"   (ppHex 8 . iont_size_heap_commit)
       , ppf 4  "loader flags"          (ppHex 8 . iont_loader_flags)
       , ppf 4  "rva num and sizes"     (ppHex 8 . iont_rva_num_and_sizes)
       ]

ppImageDataDirectory :: String -> ImageDataDirectory -> Doc
ppImageDataDirectory s a =
    tableProlog s (24,6) (sequence fields a) 
  where
    ppf    = ppField 4 24
    fields = 
       [ ppf 4  "virtual address"       (ppHex 8 . idd_virtual_addr)
       , ppf 4  "size"                  (ppHex 8 . idd_size)
       ]


ppSectionHeader :: SectionHeader -> Doc
ppSectionHeader a = 
    tableProlog "SECTION HEADER" (24,6) (sequence fields a)
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
    $+$ ppExportAddressTable   (ed_export_address_table a)
    $+$ ppExportNamePtrTable   (ed_name_ptr_table a)
    $+$ ppExportOrdinalTable   (ed_ordinal_table a)
    $+$ ppExportNames          (ed_name_table a)

ppExportDirectoryTable :: ExportDirectoryTable -> Doc
ppExportDirectoryTable a = 
    tableProlog ".edata (Export directory table)" (24,6) (sequence fields a)
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

ppExportAddressTable :: [ExportAddress] -> Doc
ppExportAddressTable a = 
    tableProlog "Export address" (24,6) (map field a)
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
    $+$ text s 
    $+$ columnSep
    $+$ columnHeadings m n
    $+$ columnSep
    $+$ vcat ds
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
listDoc = brackets . hcat . punctuate comma

