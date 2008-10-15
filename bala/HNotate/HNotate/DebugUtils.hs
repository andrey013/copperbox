

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

import HNotate.BuildNoteList
import HNotate.CommonUtils
import HNotate.Env
import HNotate.NoteListDatatypes
import HNotate.OutputMain
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (StParser, Token, parseFromFileState, streamTokens)
import HNotate.TemplateDatatypes

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (ParseError)
import Text.PrettyPrint.Leijen


dumpLyTemplates :: FilePath -> IO ()
dumpLyTemplates path = do
    putStrLn "LilyPond:"
    putStrLn "------------"
    dumpTemplates lyTextChunks preprocessLy path 

dumpAbcTemplates :: FilePath -> IO ()
dumpAbcTemplates path = do
    putStrLn "Abc:"
    putStrLn "------------"
    dumpTemplates abcTextChunks preprocessAbc path 

-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
dumpTemplates textualParse prepro path = do
    putStrLn " - Textual view:"
 --   textualParse path >>= either (putStrLn . show) (putDoc80 . pretty)
    putStrLn " - After preprocessing:"
    extraction prepro path




extraction :: (FilePath -> IO (Either ParseError String)) -> FilePath -> IO ()
extraction prepro inf = prepro inf >>= either print putStrLn

dumpLyScoreZero :: System -> FilePath -> IO ()
dumpLyScoreZero sys filepath = putStrLn "TODO"

dumpAbcScoreZero :: System -> FilePath -> IO ()
dumpAbcScoreZero sys filepath = putStrLn "TODO"
    
    {-
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
        let idxp = undefined -- runOutputReader (buildIndexedPlugs $ getExprs ev) config env
        in putDoc80 $ text "todo" -- ppIndexedPlugs idxp


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
                              
-}
                                                
