
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
import HNotate.EventInterface
import HNotate.EventList
import HNotate.ExtractionDatatypes
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.Pitch
import HNotate.TextAbc (getAbc)
import HNotate.TextLilyPond (getLy)
import HNotate.ToNoteList


import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import Text.PrettyPrint.Leijen

data OutputFormat = Output_Abc | Output_LilyPond | Output_Midi 
  deriving (Eq,Show) 
  
  
data Env = Env { 
    output_format       :: OutputFormat,
    key                 :: Key, 
    meter               :: Meter, 
    default_note_length :: Duration, 
    relative_pitch      :: Pitch,
    partial_measure     :: (Int,Int)
  }
  deriving (Show)

 
data Instruction = Cmd Command
                 | Eval MetaDirective
                 | BeginLocal
                 | EndLocal
  deriving (Show)

type Code = Seq Instruction

default_env = Env {
    output_format           = Output_LilyPond, 
    key                     = c_major,
    meter                   = four_four,
    default_note_length     = quarter,
    relative_pitch          = middle_c,
    partial_measure         = (0,0)
  }




-- to do `schemes`
relative = translateLilyPond

outputLilyPond :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputLilyPond sys infile outfile = 
  output Output_LilyPond sys lySPV lyPIV infile outfile

outputAbc :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputAbc sys infile outfile = 
    output Output_Abc sys abcSPV abcPIV infile outfile

   
   
-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
output fmt sys spvParse pivParse infile outfile = 
    successFailM (spvParse infile) sk1 fk 
  where
    fk  err      = putStrLn $ show err
    sk1 ans      = successFailM (pivParse infile) (sk2 ans) fk
    sk2 spv piv  = outputDoc outfile (build fmt sys spv piv)

        
build :: Event evt => OutputFormat -> System evt -> SPV -> PIV -> Doc
build fmt sys spv (PIV xs)  = 
    let mp = Map.fromList $ evaluateSrcExprs fmt sys xs
    in fillSourceHoles fmt mp spv


  
fillSourceHoles :: OutputFormat -> Map.Map Int Doc -> SPV -> Doc
fillSourceHoles fmt mp (SPV se) = F.foldl fn empty se
  where 
    fn d (SourceText ss)          = d <> string ss
    fn d (MetaMark i _ _)         = maybe (fk d i) (sk d) (Map.lookup i mp)
       
    fk d i  = d <> comment ("Failed to find " ++ show i)
    sk d nl = d <> pretty nl
    
    comment = if fmt == Output_Abc then abcComment else lyComment   

lyComment str = enclose (text "%{ ") (text " %}") (string str)             
abcComment str = line <> char '%' <+> string str <> line


evaluateSrcExprs :: Event evt => 
    OutputFormat -> System evt -> [ScoreElement] -> [(Idx,Doc)]
evaluateSrcExprs fmt sys []      = []
evaluateSrcExprs fmt sys (x:xs)  = 
    snd $ workE sys (default_env {output_format=fmt}) [] x xs

workE :: Event evt => System evt -> Env -> [(Idx,Doc)] -> 
            ScoreElement -> [ScoreElement] -> (Env,[(Idx,Doc)])
workE sys env code expr []     = evaluate sys env code expr
workE sys env code expr (x:xs) = let (env',code') = evaluate sys env code expr
                             in workE sys env' code' x xs

evaluate sys env code (Command cmd)         = (updateEnv cmd env, code)
evaluate sys env code (Directive idx drct)  = 
    (env, (directive sys env idx drct) : code)
evaluate sys env code (Nested [])           = (env,code)
evaluate sys env code (Nested (x:xs))       = 
    let (_,code') = workE sys env code x xs in (env,code')


directive sys env i (MetaOutput name "relative") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist (mkEnv $ default_note_length env)
                 in (i, getLy $ translateLilyPond sc middleC)
                 
    mkEnv d = ProgressEnv { measure_length = d }


directive sys env i (MetaOutput name "default") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist (mkEnv $ default_note_length env)
                 in (i, getAbc $ translateAbc sc quarter)
                 
    mkEnv d = ProgressEnv { measure_length = d }
    

updateEnv :: Command -> Env -> Env
updateEnv (CmdKey k)                env = env {key = k}
updateEnv (CmdMeter m)              env = env {meter = m}
updateEnv (CmdDefaultNoteLength d)  env = env {default_note_length = d}
updateEnv (CmdRelativePitch p)      env = env {relative_pitch = p}
updateEnv (CmdPartial a b)          env = env {partial_measure = (a,b) }



    
          
          


    