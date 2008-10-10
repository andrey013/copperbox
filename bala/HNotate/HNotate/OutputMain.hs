
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
import HNotate.BuildNoteList
import HNotate.CommonUtils (outputDoc)
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteList
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.TemplateDatatypes

import Control.Applicative hiding (empty)
import Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Sequence hiding (empty)
import Text.ParserCombinators.Parsec (ParseError)
import Text.PrettyPrint.Leijen hiding ( (<$>) ) 



type IndexedPlugs = Map.Map Int Doc

lookupPlug :: Int -> IndexedPlugs -> Maybe Doc
lookupPlug = Map.lookup


type Scheme = Int -> EventList -> Env -> Plug  

data PlugScheme = PlugScheme {
    defaultPS   :: Scheme,
    relativePS  :: Scheme
  }

data Config = Config {
    sys               :: System,
    plug_scheme       :: PlugScheme,
    textual_parser    :: FilePath -> IO (Either ParseError TextualView),
    expr_parser       :: FilePath -> IO (Either ParseError ExprView)
  }
  

type OutputReaderM a = ReaderT Env (Reader Config) a


runOutputReader :: OutputReaderM a -> Config -> Env -> a
runOutputReader f config env = runReader (runReaderT f env) config


default_ly_config system  = Config {
    sys             = system,
    plug_scheme     = psFullTranslation,
    textual_parser  = lyTextualView,
    expr_parser     = lyExprView_TwoPass
  }
    
default_abc_config system  = Config {
    sys             = system,
    plug_scheme     = psFullTranslation,
    textual_parser  = abcTextualView,
    expr_parser     = abcExprView_TwoPass
  }

set_plug_scheme :: PlugScheme -> Config -> Config
set_plug_scheme scm env = env { plug_scheme = scm }


psFullTranslation :: PlugScheme
psFullTranslation = PlugScheme { defaultPS   = abcDefault,
                                 relativePS  = lyRelative }  
  where 
    abcDefault :: Scheme
    abcDefault i evtlist env' = Plug i $ 
        translateAbc (toNoteList evtlist env') env'
    
    lyRelative :: Scheme
    lyRelative i evtlist env' = Plug i $ 
        translateLilyPond (toNoteList evtlist env') env'


-- {NOTE} Some elements in the env don't really have defaults
-- that might be too arbitrary (e.g. meter pattern)
   
outputLilyPond :: System -> FilePath -> FilePath -> IO ()
outputLilyPond sys infile outfile = 
  output sys default_ly_env infile outfile

outputAbc :: System -> FilePath -> FilePath -> IO ()
outputAbc sys infile outfile = 
    output sys default_abc_env infile outfile
    
-- output is the most general output function    
output :: System -> Env -> FilePath -> FilePath -> IO ()
output sys env infile outfile = case output_format env of
    Output_LilyPond -> outputStep (default_ly_config sys)  env infile outfile
    Output_Abc      -> outputStep (default_abc_config sys) env infile outfile
                             


outputStep :: Config -> Env -> FilePath ->  FilePath -> IO ()
outputStep config env infile outfile = 
    let textParse   = textual_parser config
        expParse    = expr_parser config
    in textParse infile >>= either failure (step2 expParse) 
  where
    failure err             = putStrLn $ show err
    step2 expParse text_rep =  expParse infile >>= 
                               either failure (step3 text_rep)
    step3 text_rep exp_rep  = outputDoc outfile $ 
            runOutputReader (buildOutput text_rep exp_rep) config env
    

buildOutput :: TextualView -> ExprView -> OutputReaderM Doc
buildOutput text_rep expr_rep  = do
    idxp        <- buildIndexedPlugs (getExprs expr_rep)
    commentFun  <- asks score_comment
    return $ fillSourceHoles commentFun idxp text_rep
   
-- Usefully a top level function - used by DebugUtils   
buildIndexedPlugs :: [Expr] -> OutputReaderM IndexedPlugs
buildIndexedPlugs xs  = foldr addplug Map.empty <$> buildPlugs xs
  where    
    addplug :: Plug -> IndexedPlugs -> IndexedPlugs
    addplug (Plug i doc) mp = Map.insert i doc mp
    
  
fillSourceHoles :: (String -> Doc) -> IndexedPlugs -> TextualView -> Doc
fillSourceHoles comment idxp tv = F.foldl fn empty $ getTextElements tv
  where 
    fn d (SourceText ss)      = d <> string ss
    fn d (MetaMark i pos _)   = maybe (fk d i) (sk d) (lookupPlug i idxp)
       
    fk d i  = d <> comment ("Failed to find " ++ show i)
    sk d nl = d <> (align $ pretty nl)
   

buildPlugs :: [Expr] -> OutputReaderM [Plug]
buildPlugs xs      = F.foldlM eval [] xs

eval :: [Plug] -> Expr -> OutputReaderM [Plug]
eval acc (LetExpr f es) = F.foldlM (\acc' e -> local f (eval acc' e)) acc es

    
eval acc (Action i d)   = (\a -> a:acc) <$> directive i d

directive :: Idx -> MetaDirective -> OutputReaderM Plug
directive i (MetaOutput scm sys_name) = outputScheme i scm sys_name
    
    
-- this one is pending a cleanup...
outputScheme :: Idx -> OutputScheme -> SystemIndex -> OutputReaderM Plug
outputScheme i LyRelative sys_name = do
    sys'     <- lift $ asks sys
    pscheme' <- lift $ asks plug_scheme
    env'     <- ask
    case (Map.lookup sys_name sys') of
      Just evts -> return $ (relativePS pscheme') i evts env'
      Nothing   -> error $ "output failure - missing " ++ sys_name
    
outputScheme i AbcDefault sys_name   = do
    sys'     <- lift $ asks sys
    pscheme' <- lift $ asks plug_scheme
    env'     <- ask
    case (Map.lookup sys_name sys') of
      Just evts -> return $ (defaultPS pscheme') i evts env'
      Nothing   -> error $ "output failure - missing " ++ sys_name

 
