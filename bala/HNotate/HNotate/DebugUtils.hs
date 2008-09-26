

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
import HNotate.NoteList
import HNotate.OutputMain
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (StParser, Token, parseFromFileState, streamTokens)
import HNotate.PreprocessTemplate
import HNotate.TemplateDatatypes
import HNotate.ToNoteList

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen


dumpLyTemplates :: FilePath -> IO ()
dumpLyTemplates path = do
    putStrLn "LilyPond:"
    putStrLn "------------"
    dumpTemplates lyTextualView lyPrePro path 

dumpAbcTemplates :: FilePath -> IO ()
dumpAbcTemplates path = do
    putStrLn "Abc:"
    putStrLn "------------"
    dumpTemplates abcTextualView abcPrePro path 

-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
dumpTemplates textualParse prepro path = do
    putStrLn " - Textual view:"
    textualParse path >>= either (putStrLn . show) (putDoc80 . pretty)
    putStrLn " - After preprocessing:"
    extraction prepro path




extraction :: StParser [Token] -> FilePath -> IO ()
extraction prepro inf =
    parseFromFileState prepro inf 0 >>= either print (putStrLn . streamTokens)

    
    
    
dumpLyScoreZero :: System -> FilePath -> IO ()
dumpLyScoreZero sys filepath =
    let cfg = set_plug_scheme psDebug $ default_ly_config sys
    in dumpScoreZero cfg default_ly_env filepath
    
    
dumpAbcScoreZero :: System -> FilePath -> IO ()
dumpAbcScoreZero sys filepath =
    let cfg = set_plug_scheme psDebug $ default_abc_config sys
    in dumpScoreZero cfg default_abc_env filepath


dumpScoreZero config env filepath  =  
    (expr_parser config) filepath >>= either failure showPlugs
  where
    failure   err  = putStrLn $ show err    
    showPlugs ev   = 
        let idxp = runOutputReader (buildIndexedPlugs $ getExprs ev) config env
        in putDoc80 $ ppIndexedPlugs idxp


psDebug :: PlugScheme           
psDebug = PlugScheme {
    defaultPS   = oneStep,
    relativePS  = oneStep
  }

oneStep :: Int -> EventList -> Env -> Plug   
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


instance Pretty ScoreGlyph where
  pretty (SgNote pch dur)         = pretty pch <> durationSuffix dur
  pretty (SgRest dur)             = char 'r' <> durationSuffix dur
  pretty (SgSpacer dur)           = char 's' <> durationSuffix dur
  pretty (SgChord ps dur)         = (brackets $ sepSeq (<>) ps) 
                                      <> durationSuffix dur
  pretty (SgGraceNotes es)        = braces $ sepSeq (<>) es

  pretty (SgBeamStart)            = text "[["
  pretty (SgBeamEnd)              = text "]]" <> line
  pretty (SgTie)                  = text "~~"
                

durationSuffix :: Pretty drn => drn -> Doc
durationSuffix d = char '/' <> pretty d 


intPlex i = let s = show i in (s,length s)

tagint len i = let (s,l) = intPlex i in
  if l < len then text (replicate (len -l) '0' ++ s) else text s
  
--------------------------------------------------------------------------------
-- Template files

ppPos :: SrcPos -> Doc 
ppPos (SrcPos l c _) = pretty $ (l,c)

instance Pretty TextualView where
  pretty (TextualView se) = F.foldl (\d e -> d <> pretty e) empty se <> line


instance Pretty TextElement where
  pretty (SourceText str)     = string str
  pretty (MetaMark idx pos d) = hshComment (ppPos pos <+> int idx <+> pretty d)

  
instance Pretty MetaDirective where
  pretty (MetaOutput scm idx) = pretty scm <> colon <> text idx
  
instance Pretty OutputScheme where
  pretty LyRelative = text "relative" 
  pretty AbcDefault = text "default"
  
    
 
                                                
