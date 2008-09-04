
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
import System.IO
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
  output Output_LilyPond sys parseLySourceChunks parseLyTemplateExpr infile outfile

outputAbc :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputAbc sys infile outfile = 
    output Output_Abc sys parseAbcSourceChunks parseAbcTemplateExpr infile outfile

   
   
-- output :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
output fmt sys p1 p2 infile outfile = 
    workO (p1 infile) sk1 fk 
  where
    fk  err           = putStrLn $ show err
    sk1 ans           = workO (p2 infile) (sk2 ans) fk
    sk2 chunks exprs  = outputDoc outfile (build fmt sys chunks exprs)

 
    


workO :: Monad m => m (Either a b) -> (b -> m c) -> (a -> m c) -> m c
workO p sk fk = p >>= either fk sk

        
build :: Event evt => OutputFormat -> System evt -> SourceFile -> [SrcExpr] -> Doc
build fmt sys chunks exprs  = 
    let mp = Map.fromList $ evaluateSrcExprs fmt sys exprs
    in fillSourceHoles fmt mp chunks

outputDoc :: FilePath -> Doc -> IO ()
outputDoc filepath doc = do
    h <- openFile filepath WriteMode
    displayIO h (renderPretty 0.7 80 doc)
    hClose h
  
fillSourceHoles :: OutputFormat -> Map.Map Int Doc -> SourceFile -> Doc
fillSourceHoles fmt mp (SourceFile se) = F.foldl fn empty se
  where 
    fn d (SourceText ss)                 = d <> string ss
    fn d (MetaMark _ (MetaOutput i _ _)) = maybe (fk d i) (sk d) (Map.lookup i mp)
       
    fk d i  = d <> comment ("Failed to find " ++ show i)
    sk d nl = d <> pretty nl
    
    comment = if fmt == Output_Abc then abcComment else lyComment   

lyComment str = enclose (text "%{ ") (text " %}") (string str)             
abcComment str = line <> char '%' <+> string str <> line


evaluateSrcExprs :: Event evt => 
    OutputFormat -> System evt -> [SrcExpr] -> [(DId,Doc)]
evaluateSrcExprs fmt sys []      = []
evaluateSrcExprs fmt sys (x:xs)  = 
    snd $ workE sys (default_env {output_format=fmt}) [] x xs

workE :: Event evt => System evt -> Env -> [(DId,Doc)] -> 
            SrcExpr -> [SrcExpr] -> (Env,[(DId,Doc)])
workE sys env code expr []     = evaluate sys env code expr
workE sys env code expr (x:xs) = let (env',code') = evaluate sys env code expr
                             in workE sys env' code' x xs

evaluate sys env code (Command cmd)     = (updateEnv cmd env, code)
evaluate sys env code (Directive drct)  = (env, (directive sys env drct) : code)
evaluate sys env code (Nested [])       = (env,code)
evaluate sys env code (Nested (x:xs))   = 
    let (_,code') = workE sys env code x xs in (env,code')


directive sys env (MetaOutput i name "relative") =
    maybe failure  sk (Map.lookup name sys)
  where
    failure = error $ "directive failure - missing " ++ name
  
    sk evtlist = let sc = toNoteList evtlist (mkEnv $ default_note_length env)
                 in (i, getLy $ translateLilyPond sc middleC)
                 
    mkEnv d = ProgressEnv { measure_length = d }


directive sys env (MetaOutput i name "default") =
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



    
          
          


    