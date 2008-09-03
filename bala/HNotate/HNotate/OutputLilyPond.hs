
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
import HNotate.EventTree
import HNotate.ExtractionDatatypes
import HNotate.ParseLy
import HNotate.Pitch
import HNotate.ToScore
import HNotate.ScoreRepresentation

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import System.IO
import Text.PrettyPrint.Leijen


type Env = (Key, Meter, Duration, Pitch)

 
data Instruction = Cmd Command
                 | Eval MetaDirective
                 | BeginLocal
                 | EndLocal
  deriving (Show)

type Code = Seq Instruction


-- to do `schemes`
relative = translateLilyPond

outputLilyPond sys infile outfile = do
  a <- parseLySourceChunks infile
  case a of 
    Left err -> putStrLn $ show err
    Right chunks -> do
      b <- parseLyTemplateExpr infile
      case b of 
        Left err -> putStrLn $ show err
        Right exprs -> let out = build sys chunks exprs in do
                          h <- openFile outfile WriteMode
                          displayIO h (renderPretty 0.7 80 out)
                          hClose h
 
        
build :: Event evt => System evt -> SourceFile -> [SrcExpr] -> Doc
build sys chunks exprs  = 
    let mp = Map.fromList $ evaluateSrcExprs sys exprs
    in output mp chunks
  
output :: Map.Map Int LilyPondNoteList -> SourceFile -> Doc
output mp (SourceFile se) = F.foldl fn empty se
  where fn d (SourceText ss) = d <> string ss
        fn d (MetaMark _ (MetaOutput i _ _)) = case Map.lookup i mp of
            Just notes -> d <> pretty notes
            Nothing -> d <> lyComment ("Failed to find " ++ show i)

lyComment str = enclose (text "%{ ") (text " %}") (string str)             

default_env = (c_major,four_four,quarter,middle_c)

evaluateSrcExprs :: Event evt 
                 => System evt -> [SrcExpr] -> [(DId,LilyPondNoteList)]
evaluateSrcExprs sys []      = []
evaluateSrcExprs sys (x:xs)  = snd $ workE sys default_env [] x xs

workE :: Event evt => System evt -> Env -> [(DId,LilyPondNoteList)] -> 
            SrcExpr -> [SrcExpr] -> (Env,[(DId,LilyPondNoteList)])
workE sys env code expr []     = evaluate sys env code expr
workE sys env code expr (x:xs) = let (env',code') = evaluate sys env code expr
                             in workE sys env' code' x xs

evaluate sys env code (Command cmd)     = (updateEnv env cmd, code)
evaluate sys env code (Directive drct)  = (env, (directive sys env drct) : code)
evaluate sys env code (Nested [])       = (env,code)
evaluate sys env code (Nested (x:xs))   = 
    let (_,code') = workE sys env code x xs in (env,code')


directive sys env (MetaOutput i name "relative") = 
    let score = toScore sys (mkEnv $ lookupDuration env) -- wrong shouldn't translate whole system
        notes = onNoteList score "bulgarian6" $ \se -> 
                                translateLilyPond se middleC
    in case notes of
                  Just a -> (i,a)
                  Nothing -> error $ "directive - failure"

  where
    mkEnv d = ProgressEnv { measure_length = d }

 
updateEnv env (CmdKey k)               = setKey k env
updateEnv env (CmdMeter m)             = setMeter m env
updateEnv env (CmdDefaultNoteLength d) = setDuration d env
updateEnv env (CmdRelativePitch p)     = setPitch p env
    


lookupKey :: Env -> Key
lookupKey (k,_,_,_) = k

lookupMeter :: Env -> Meter
lookupMeter (_,m,_,_) = m

lookupDuration :: Env -> Duration
lookupDuration (_,_,d,_) = d

lookupPitch :: Env -> Pitch
lookupPitch (_,_,_,p) = p


setKey :: Key -> Env -> Env
setKey k (_,m,d,p) = (k,m,d,p)

setMeter :: Meter -> Env -> Env
setMeter m (k,_,d,p) = (k,m,d,p)

setDuration :: Duration -> Env -> Env
setDuration d (k,m,_,p) = (k,m,d,p)

setPitch :: Pitch -> Env -> Env
setPitch p (k,m,d,_) = (k,m,d,p)



    
    
          
          


    