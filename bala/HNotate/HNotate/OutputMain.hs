
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
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteList
import HNotate.ParseAbc
import HNotate.ParseLy
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


type Scheme = Int -> EventList -> Env -> Plug  

data PlugScheme = PlugScheme {
    defaultPS   :: Scheme,
    relativePS  :: Scheme
  }

abcDefault :: Scheme
abcDefault i evtlist env = Plug i $ 
    getAbc $ translateAbc (toNoteList evtlist env) env

lyRelative :: Scheme
lyRelative i evtlist env = Plug i $ 
    getLy $ translateLilyPond (toNoteList evtlist env) env

psFullTranslation :: PlugScheme
psFullTranslation = PlugScheme {
    defaultPS   = abcDefault,
    relativePS  = lyRelative
  }  

    
outputLilyPond :: System -> FilePath -> FilePath -> IO ()
outputLilyPond sys infile outfile = 
  output default_ly_env sys psFullTranslation lySPV lyPIV infile outfile

outputAbc :: System -> FilePath -> FilePath -> IO ()
outputAbc sys infile outfile = 
    output default_abc_env sys psFullTranslation abcSPV abcPIV infile outfile

   
   
-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
output env sys pscheme spvParse pivParse infile outfile = 
    successFailM (spvParse infile) sk1 fk 
  where
    fk  err      = putStrLn $ show err
    sk1 ans      = successFailM (pivParse infile) (sk2 ans) fk
    sk2 spv piv  = outputDoc outfile (buildOutput env sys pscheme spv piv)

        
buildOutput :: Env -> System -> PlugScheme -> SPV -> PIV -> Doc
buildOutput env sys pscheme spv piv  = 
    let idxp = buildIndexedPlugs env sys pscheme piv
    in fillSourceHoles (score_comment env) idxp spv

buildIndexedPlugs :: Env -> System -> PlugScheme -> PIV -> IndexedPlugs
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
   



buildPlugs :: Env -> System -> PlugScheme -> [ScoreElement] -> [Plug]
buildPlugs env sys pscheme []      = []
buildPlugs env sys pscheme (x:xs)  = snd $ workEval env sys pscheme [] x xs

workEval :: Env -> System -> PlugScheme -> [Plug] -> 
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



directive :: 
    Env -> Map.Map Name EventList -> PlugScheme -> Idx -> MetaDirective -> Plug
directive env sys pscheme i (MetaOutput name scheme_name) =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let scheme = useScheme scheme_name pscheme
                 in scheme i evtlist env
                 
useScheme :: String -> PlugScheme -> Scheme
useScheme "relative"  pscheme = relativePS pscheme
useScheme "default"   pscheme = defaultPS pscheme



updateEnv :: Command -> Env -> Env
updateEnv (CmdKey k)                env = set_current_key k env
updateEnv (CmdMeter m)              env = set_current_meter m env
updateEnv (CmdUnitNoteLength d)     env = set_unit_note_length d env
updateEnv (CmdRelativePitch p)      env = set_relative_pitch p env
updateEnv (CmdPartialMeasure d)     env = set_partial_measure d env
updateEnv (CmdCadenzaOn)            env = set_cadenza True env
updateEnv (CmdCadenzaOff)           env = set_cadenza False env


    