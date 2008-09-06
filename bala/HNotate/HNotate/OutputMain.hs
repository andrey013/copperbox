
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
    sk2 spv piv  = outputDoc outfile (build env sys spv piv)

        
build :: Event evt => Env -> System evt -> SPV -> PIV -> Doc
build env sys spv (PIV xs)  = 
    let mp = Map.fromList $ evaluateSrcExprs env sys xs
    in fillSourceHoles env mp spv


  
fillSourceHoles :: Env -> Map.Map Int Doc -> SPV -> Doc
fillSourceHoles env mp (SPV se) = F.foldl fn empty se
  where 
    fn d (SourceText ss)      = d <> string ss
    fn d (MetaMark i pos _)   = maybe (fk d i) (sk d) (Map.lookup i mp)
       
    fk d i  = d <> comment ("Failed to find " ++ show i)
    sk d nl = d <> (align $ pretty nl)
    
    comment = score_comment env   



evaluateSrcExprs :: Event evt => 
    Env -> System evt -> [ScoreElement] -> [(Idx,Doc)]
evaluateSrcExprs env sys []      = []
evaluateSrcExprs env sys (x:xs)  = 
    snd $ workEval env sys [] x xs

workEval :: Event evt => Env -> System evt -> [(Idx,Doc)] -> 
            ScoreElement -> [ScoreElement] -> (Env,[(Idx,Doc)])
workEval env sys code expr []     = evaluate env sys code expr
workEval env sys code expr (x:xs) = 
    let (env',code') = evaluate env sys code expr
    in workEval env' sys code' x xs

evaluate env sys code (Command cmd)         = (updateEnv cmd env, code)
evaluate env sys code (Directive idx drct)  = 
    (env, (directive env sys idx drct) : code)
evaluate env sys code (Nested [])           = (env,code)
evaluate env sys code (Nested (x:xs))       = 
    let (_,code') = workEval env sys code x xs in (env,code')


directive env sys i (MetaOutput name "relative") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist env
                 in (i, getLy $ translateLilyPond sc (relative_pitch env))
                 



directive env sys i (MetaOutput name "default") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist env
                 in (i, getAbc $ translateAbc sc (unit_note_length env))

    

updateEnv :: Command -> Env -> Env
updateEnv (CmdKey k)                env = env {current_key = k}
updateEnv (CmdMeter m)              env = updateMeter m env
updateEnv (CmdDefaultNoteLength d)  env = env {unit_note_length = d}
updateEnv (CmdRelativePitch p)      env = env {relative_pitch = p}
updateEnv (CmdPartial a b)          env = env {partial_measure = (a,b) }



    
          
          


    