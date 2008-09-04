
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.OutputLilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output LilyPond according to metadirectives in .ly file. 
--
--------------------------------------------------------------------------------


module HNotate.OutputLilyPond where

import HNotate.BackendLilyPond
import HNotate.Duration
import HNotate.EventInterface
import HNotate.EventList
import HNotate.ExtractionDatatypes
import HNotate.NoteListDatatypes
import HNotate.ParseLy
import HNotate.Pitch
import HNotate.ToNoteList


import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import System.IO
import Text.PrettyPrint.Leijen


data Env = Env { 
    key                 :: Key, 
    meter               :: Meter, 
    default_note_length :: Duration, 
    relative_pitch      :: Pitch,
    partial_measure     :: (Int,Int)
  }

 
data Instruction = Cmd Command
                 | Eval MetaDirective
                 | BeginLocal
                 | EndLocal
  deriving (Show)

type Code = Seq Instruction

default_env = Env { 
    key                     = c_major,
    meter                   = four_four,
    default_note_length     = quarter,
    relative_pitch          = middle_c,
    partial_measure         = (0,0)
  }




-- to do `schemes`
relative = translateLilyPond

outputLilyPond :: (Event evt) => System evt -> FilePath -> FilePath -> IO ()
outputLilyPond sys infile outfile = workO (parseLySourceChunks infile) sk1 fk 
  where
    fk  err           = putStrLn $ show err
    sk1 ans           = workO (parseLyTemplateExpr infile) (sk2 ans) fk
    sk2 chunks exprs  = outputDoc outfile (build sys chunks exprs)

workO :: Monad m => m (Either a b) -> (b -> m c) -> (a -> m c) -> m c
workO p sk fk = p >>= either fk sk

        
build :: Event evt => System evt -> SourceFile -> [SrcExpr] -> Doc
build sys chunks exprs  = 
    let mp = Map.fromList $ evaluateSrcExprs sys exprs
    in output mp chunks

outputDoc :: FilePath -> Doc -> IO ()
outputDoc filepath doc = do
    h <- openFile filepath WriteMode
    displayIO h (renderPretty 0.7 80 doc)
    hClose h
  
output :: Map.Map Int LilyPondNoteList -> SourceFile -> Doc
output mp (SourceFile se) = F.foldl fn empty se
  where fn d (SourceText ss) = d <> string ss
        fn d (MetaMark _ (MetaOutput i _ _)) = case Map.lookup i mp of
            Just notes -> d <> pretty notes
            Nothing -> d <> lyComment ("Failed to find " ++ show i)

lyComment str = enclose (text "%{ ") (text " %}") (string str)             



evaluateSrcExprs :: Event evt 
                 => System evt -> [SrcExpr] -> [(DId,LilyPondNoteList)]
evaluateSrcExprs sys []      = []
evaluateSrcExprs sys (x:xs)  = snd $ workE sys default_env [] x xs

workE :: Event evt => System evt -> Env -> [(DId,LilyPondNoteList)] -> 
            SrcExpr -> [SrcExpr] -> (Env,[(DId,LilyPondNoteList)])
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
                 in (i, translateLilyPond sc middleC)
                 
    mkEnv d = ProgressEnv { measure_length = d }

updateEnv :: Command -> Env -> Env
updateEnv (CmdKey k)                env = env {key = k}
updateEnv (CmdMeter m)              env = env {meter = m}
updateEnv (CmdDefaultNoteLength d)  env = env {default_note_length = d}
updateEnv (CmdRelativePitch p)      env = env {relative_pitch = p}
updateEnv (CmdPartial a b)          env = env {partial_measure = (a,b) }



    
          
          


    