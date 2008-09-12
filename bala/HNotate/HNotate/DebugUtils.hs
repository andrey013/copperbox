

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DebugUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Debugging helpers - pretty printers of various internal formats.
--
--------------------------------------------------------------------------------


module HNotate.DebugUtils where

import HNotate.CommonUtils
import HNotate.Env
import HNotate.EventInterface
import HNotate.EventList
import HNotate.NoteListDatatypes
import HNotate.OutputMain
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.TemplateDatatypes
import HNotate.ToNoteList

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen


dumpLyTemplates :: FilePath -> IO ()
dumpLyTemplates = dumpTemplates lySPV lyPIV

dumpAbcTemplates :: FilePath -> IO ()
dumpAbcTemplates = dumpTemplates abcSPV abcPIV

-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
dumpTemplates chunkParse islandParse filepath = do
    successFailM (chunkParse filepath) sk fk
    putStrLn ""
    successFailM (islandParse filepath) sk fk
  where
    fk = putStrLn . show
    sk :: Pretty a => a -> IO ()
    sk = putDoc80 . pretty

dumpLyScoreZero :: (Event evt) => System evt -> FilePath -> IO ()
dumpLyScoreZero sys filepath = dumpScoreZero sys lyPIV filepath

dumpAbcScoreZero :: (Event evt) => System evt -> FilePath -> IO ()
dumpAbcScoreZero sys filepath = dumpScoreZero sys abcPIV filepath

dumpScoreZero sys parsePiv filepath = 
    successFailM (parsePiv filepath) sk fk
  where
    fk err    = putStrLn $ show err    
    sk piv    = let idxp = buildIndexedPlugs default_ly_env sys psDebug piv
                in putDoc80 $ ppIndexedPlugs idxp

psDebug :: Event evt => PlugScheme evt           
psDebug = PlugScheme {
    defaultPS   = oneStep,
    relativePS  = oneStep
  }

oneStep :: Event evt => Int -> EventList evt -> Env -> Plug   
oneStep i evtlist env = Plug i $ 
    pretty $ toNoteList evtlist env
    
        
ppIndexedPlugs :: IndexedPlugs -> Doc
ppIndexedPlugs = vsep . (map ppEvtsPair) . Map.toAscList 
  where
    ppEvtsPair (i,notelist) = text "Notelist" <+> int i <> colon <$>
                              pretty notelist <> line
                              
--------------------------------------------------------------------------------
-- pretty printing

hshComment :: Doc -> Doc
hshComment = enclose (text "{-# ") (text " #-}")

instance (Pretty e) => Pretty (ScNoteList e) where
  pretty (ScNoteList se) = sepSeq (<$>) se



instance (Pretty e) => Pretty (ScBlock e) where
  pretty (ScSingleBlock i e) = measureNumber i
                                         <$> indent 4 (pretty e)
  pretty (ScPolyBlock i se)  = 
      measureNumber i <$> indent 4 (encloseSep (text "<< ") 
                                               (text " >>") 
                                               (text " // ")
                                               (map pretty $ F.toList se))

measureNumber :: Int -> Doc
measureNumber i = text "|:" <>  int i


instance (Pretty e) => Pretty (ScMeasure e) where
  pretty (ScMeasure se) = sepSeq (</>) se


instance (Pretty pch, Pretty drn) => Pretty (Glyph pch drn) where
  pretty (GlyNote pch dur)       = pretty pch <> durationSuffix dur
  pretty (GlyRest dur)           = char 'r' <> durationSuffix dur
  pretty (GlySpacer dur)         = char 's' <> durationSuffix dur
  pretty (GlyChord ps dur)       = (brackets $ sepSeq (<>) ps) 
                                      <> durationSuffix dur
  pretty (GlyGraceNotes es)      = text "grace..." -- braces $ sepSeq (<>) ps

durationSuffix :: Pretty drn => drn -> Doc
durationSuffix d = char '/' <> pretty d 


intPlex i = let s = show i in (s,length s)

tagint len i = let (s,l) = intPlex i in
  if l < len then text (replicate (len -l) '0' ++ s) else text s
  
--------------------------------------------------------------------------------
-- Template files

ppPos :: SrcPos -> Doc 
ppPos (SrcPos l c _) = pretty $ (l,c)

instance Pretty SPV where
  pretty (SPV se) = 
      spv_prefix <$> F.foldl (\d e -> d <> pretty e) empty se <> line
    where 
      spv_prefix = underline "Source preserving view:"

instance Pretty PIV where
  pretty (PIV xs) = piv_prefix <$> vsep (map pretty xs) <> line
    where
      piv_prefix = underline "Partially interpreted view:" 

instance Pretty TextElement where
  pretty (SourceText str)     = string str
  pretty (MetaMark idx pos d) = hshComment (ppPos pos <+> int idx <+> pretty d)


instance Pretty ScoreElement where
  pretty (Command cmd)        = text "\\cmd" <+> pretty cmd
  pretty (Directive idx drct) = hshComment (int idx <+> pretty drct)
  pretty (Nested [])          = text "{ }"   
  pretty (Nested xs)          = enclose (lbrace <> line) (line <> rbrace) 
                                        (indent 2 $ vcat $ map pretty xs)

instance Pretty Command where
  pretty (CmdKey e)               = text "-key" <+> (text . show) e
  pretty (CmdMeter e)             = text "-meter" <+> (text . show) e
  pretty (CmdUnitNoteLength e)    = text "-len" <+> (text . show) e
  pretty (CmdRelativePitch e)     = text "-rpitch" <+> (text . show) e
  pretty (CmdCadenzaOn)           = text "-cadenza_on"
  pretty (CmdCadenzaOff)          = text "-cadenza_off"
  
instance Pretty MetaDirective where
  pretty (MetaOutput name scheme) = text name <> colon <> text scheme
 
                                                
