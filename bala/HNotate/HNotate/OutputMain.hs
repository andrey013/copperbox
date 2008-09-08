
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






-- to do `schemes`
relative = translateLilyPond

outputLilyPond :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputLilyPond sys infile outfile = 
  output default_ly_env sys lySPV lyPIV infile outfile

outputAbc :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputAbc sys infile outfile = 
    output default_abc_env sys abcSPV abcPIV infile outfile

   
   
-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
output env sys spvParse pivParse infile outfile = 
    successFailM (spvParse infile) sk1 fk 
  where
    fk  err      = putStrLn $ show err
    sk1 ans      = successFailM (pivParse infile) (sk2 ans) fk
    sk2 spv piv  = outputDoc outfile (buildOutput env sys spv piv)

        
buildOutput :: Event evt => Env -> System evt -> SPV -> PIV -> Doc
buildOutput env sys spv (PIV xs)  = 
    let mp = indexPlugs $ buildPlugs env sys xs
    in fillSourceHoles (score_comment env) mp spv

-- turn Plugs into a map of Int ~> Doc 
indexPlugs :: [Plug] -> Map.Map Int Doc
indexPlugs = foldr addplug Map.empty
  where addplug (Plug i doc) mp = Map.insert i doc mp
  
fillSourceHoles :: (String -> Doc) -> Map.Map Idx Doc -> SPV -> Doc
fillSourceHoles comment mp (SPV se) = F.foldl fn empty se
  where 
    fn d (SourceText ss)      = d <> string ss
    fn d (MetaMark i pos _)   = maybe (fk d i) (sk d) (Map.lookup i mp)
       
    fk d i  = d <> comment ("Failed to find " ++ show i)
    sk d nl = d <> (align $ pretty nl)
   



buildPlugs :: Event evt => Env -> System evt -> [ScoreElement] -> [Plug]
buildPlugs env sys []      = []
buildPlugs env sys (x:xs)  = snd $ workEval env sys [] x xs

workEval :: Event evt => Env -> System evt -> [Plug] -> 
            ScoreElement -> [ScoreElement] -> (Env,[Plug])
workEval env sys code expr []     = evaluate1 env sys code expr
workEval env sys code expr (x:xs) = 
    let (env',code') = evaluate1 env sys code expr
    in workEval env' sys code' x xs

evaluate1 env sys code (Command cmd)         = (updateEnv cmd env, code)
evaluate1 env sys code (Directive idx drct)  = 
    (env, (directive env sys idx drct) : code)
evaluate1 env sys code (Nested [])           = (env,code)
evaluate1 env sys code (Nested (x:xs))       = 
    let (_,code') = workEval env sys code x xs in (env,code')

directive :: (Event evt) => Env -> Map.Map Name (EventList evt)
          -> Idx -> MetaDirective -> Plug
directive env sys i (MetaOutput name "relative") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist env
                 in Plug i (getLy $ translateLilyPond sc (relative_pitch env))
                 

directive env sys i (MetaOutput name "default") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist env
                 in Plug i (getAbc $ translateAbc sc (unit_note_length env))

    

updateEnv :: Command -> Env -> Env
updateEnv (CmdKey k)                env = update_current_key k env
updateEnv (CmdMeter m)              env = update_meter m env
updateEnv (CmdDefaultNoteLength d)  env = update_unit_note_length d env
updateEnv (CmdRelativePitch p)      env = update_relative_pitch p env
updateEnv (CmdPartial a b)          env = update_partial_measure a b env



    
          
          


    