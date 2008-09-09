
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.OutputMain
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output LilyPond or Abc according to metadirectives in the respective
--  .ly or .abc files. 
--
--------------------------------------------------------------------------------


module HNotate.OutputMain where

import HNotate.BackendAbc
import HNotate.BackendLilyPond
import HNotate.CommonUtils (successFailM, outputDoc)
import HNotate.Duration
import HNotate.Env
import HNotate.EventInterface
import HNotate.EventList
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.Pitch
import HNotate.TemplateDatatypes
import HNotate.TextAbc (getAbc)
import HNotate.TextLilyPond (getLy)
import HNotate.ToNoteList


import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import Text.PrettyPrint.Leijen


 
data Instruction = Cmd Command
                 | Eval MetaDirective
                 | BeginLocal
                 | EndLocal
  deriving (Show)

type IndexedPlugs = Map.Map Int Doc

lookupPlug :: Int -> IndexedPlugs -> Maybe Doc
lookupPlug = Map.lookup


type Scheme evt = Int -> EventList evt -> Env -> Plug  

data PlugScheme evt = PlugScheme {
    defaultPS   :: Scheme evt,
    relativePS  :: Scheme evt
  }

abcDefault :: Event evt => Scheme evt
abcDefault i evtlist env = Plug i $ 
    getAbc $ translateAbc (toNoteList evtlist env) (unit_note_length env)

lyRelative :: Event evt => Scheme evt
lyRelative i evtlist env = Plug i $ 
    getLy $ translateLilyPond (toNoteList evtlist env) (relative_pitch env)

psFullTranslation :: Event evt => PlugScheme evt
psFullTranslation = PlugScheme {
    defaultPS   = abcDefault,
    relativePS  = lyRelative
  }  

    
outputLilyPond :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputLilyPond sys infile outfile = 
  output default_ly_env sys psFullTranslation lySPV lyPIV infile outfile

outputAbc :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputAbc sys infile outfile = 
    output default_abc_env sys psFullTranslation abcSPV abcPIV infile outfile

   
   
-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
output env sys pscheme spvParse pivParse infile outfile = 
    successFailM (spvParse infile) sk1 fk 
  where
    fk  err      = putStrLn $ show err
    sk1 ans      = successFailM (pivParse infile) (sk2 ans) fk
    sk2 spv piv  = outputDoc outfile (buildOutput env sys pscheme spv piv)

        
buildOutput :: Event evt => Env -> System evt -> PlugScheme evt -> SPV -> PIV -> Doc
buildOutput env sys pscheme spv piv  = 
    let idxp = buildIndexedPlugs env sys pscheme piv
    in fillSourceHoles (score_comment env) idxp spv

buildIndexedPlugs :: Event evt => Env -> System evt -> PlugScheme evt -> PIV -> IndexedPlugs
buildIndexedPlugs env sys pscheme (PIV xs)  =
  indexPlugs $ buildPlugs env sys pscheme xs

-- turn Plugs into a map of Int ~> Doc 
indexPlugs :: [Plug] -> IndexedPlugs
indexPlugs = foldr addplug Map.empty
  where addplug (Plug i doc) mp = Map.insert i doc mp
  
fillSourceHoles :: (String -> Doc) -> IndexedPlugs -> SPV -> Doc
fillSourceHoles comment idxp (SPV se) = F.foldl fn empty se
  where 
    fn d (SourceText ss)      = d <> string ss
    fn d (MetaMark i pos _)   = maybe (fk d i) (sk d) (lookupPlug i idxp)
       
    fk d i  = d <> comment ("Failed to find " ++ show i)
    sk d nl = d <> (align $ pretty nl)
   



buildPlugs :: Event evt => Env -> System evt -> PlugScheme evt
           -> [ScoreElement] -> [Plug]
buildPlugs env sys pscheme []      = []
buildPlugs env sys pscheme (x:xs)  = snd $ workEval env sys pscheme [] x xs

workEval :: Event evt => Env -> System evt -> PlugScheme evt -> [Plug] -> 
            ScoreElement -> [ScoreElement] -> (Env,[Plug])
workEval env sys pscheme code expr []     = evaluate1 env sys pscheme code expr
workEval env sys pscheme code expr (x:xs) = 
    let (env',code') = evaluate1 env sys pscheme code expr
    in workEval env' sys pscheme code' x xs

evaluate1 env sys pscheme code (Command cmd)         = (updateEnv cmd env, code)
evaluate1 env sys pscheme code (Directive idx drct)  = 
    (env, (directive env sys pscheme idx drct) : code)
evaluate1 env sys pscheme code (Nested [])           = (env,code)
evaluate1 env sys pscheme code (Nested (x:xs))       = 
    let (_,code') = workEval env sys pscheme code x xs in (env,code')



directive :: (Event evt) => Env -> Map.Map Name (EventList evt)
          -> PlugScheme evt -> Idx -> MetaDirective -> Plug
directive env sys pscheme i (MetaOutput name scheme_name) =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let scheme = useScheme scheme_name pscheme
                 in scheme i evtlist env
                 
useScheme :: String -> PlugScheme evt -> Scheme evt
useScheme "relative"  pscheme = relativePS pscheme
useScheme "default"   pscheme = defaultPS pscheme


  

    

updateEnv :: Command -> Env -> Env
updateEnv (CmdKey k)                env = update_current_key k env
updateEnv (CmdMeter m)              env = update_meter m env
updateEnv (CmdDefaultNoteLength d)  env = update_unit_note_length d env
updateEnv (CmdRelativePitch p)      env = update_relative_pitch p env
updateEnv (CmdPartial a b)          env = update_partial_measure a b env



    
          
          


    