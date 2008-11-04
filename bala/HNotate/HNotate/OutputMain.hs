
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
import HNotate.BackendMidi
import HNotate.BuildNoteList
import HNotate.CommonUtils -- (outputDoc, showDocS)
import HNotate.Document (ODoc, formatted, output)
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (ExprParser, TextSourceParser)
import HNotate.ProcessingTypes
import HNotate.TemplateDatatypes

import Control.Applicative hiding (empty)
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence hiding (empty, reverse, length)

import Text.ParserCombinators.Parsec (ParseError, parseFromFile)



-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: Int -> System -> FilePath -> FilePath -> IO ()
outputLilyPond dl sys inpath outpath   =
    runNotateT outfun default_ly_env config           >>= \(a,msg) ->
    either (reportFailure msg) (const $ putStrLn msg) a
  where
    config  = mkLyConfig dl sys inpath outpath
    outfun  = generalOutput lyExprParse lyTextSource 
    
outputAbc :: Int -> System -> FilePath -> FilePath -> IO ()
outputAbc dl sys inpath outpath        =
    runNotateT outfun default_abc_env config          >>= \(a,msg) ->
    either (reportFailure msg) (const $ putStrLn msg) a
  where
    config  = mkAbcConfig dl sys inpath outpath
    outfun  = generalOutput abcExprParse abcTextSource

outputMidi :: Int -> System -> String -> FilePath -> IO ()
outputMidi dl sys name outpath = 
    runNotateT outfun default_abc_env config          >>= \(a,msg) ->
    either (reportFailure msg) (const $ putStrLn msg) a
  where
    config  = mkAbcConfig dl sys "" outpath
    outfun = case Map.lookup name sys of
                Nothing -> throwError $ strMsg $ "missing: " ++ name
                Just evts -> toNoteList evts >>= midiOut outpath

    
reportFailure :: String -> NotateErr -> IO ()
reportFailure log_msg (NotateErr s) = putStrLn s >> putStrLn log_msg


generalOutput :: ExprParser -> TextSourceParser -> NotateT IO ()
generalOutput expr_parser src_parser  = do 
    infile    <- asks_config _template_file
    src       <- liftIO $ parseFromFile src_parser infile
    either (fault src_fail_msg) (step2 infile) src
  where
    src_fail_msg  = "Failure running the 'text source' parser..."
    expr_fail_msg = "Failure running the 'expression' parser..." 
    
    fault msg err = textoutput 0 msg (show err) >> return ()
    
    step2 :: FilePath -> TextSource -> NotateT IO ()
    step2 infile src  =  do 
        exprs <- expr_parser infile
        either (fault expr_fail_msg) (step3 src) exprs
    
    step3 :: TextSource -> [Expr] -> NotateT IO ()
    step3 src exprs = do
        out   <- asks_config _output_file 
        fn    <- outputter exprs src
        liftIO $ writeS out fn

      

writeS :: FilePath -> ShowS -> IO ()
writeS path = writeFile path . ($ "")                                      

outputter :: Monad m => [Expr] -> TextSource -> NotateT m ShowS
outputter exprs src = evalHoas (toHoas exprs) >>= plug src



plug :: Monad m => TextSource -> [ODoc] -> NotateT m ShowS
plug (SourceFile water (Island loc rest)) (d:ds) = 
    (\ks -> showString water . output (_src_column loc) 60 d . ks) 
        <$> (plug rest ds)

plug (SourceFile water EndOfSource)     [] = return $ showString water

plug (SourceFile water EndOfSource)     ds = do 
    textoutput 0 "ERROR - 'plug'" (msg $ length ds) 
    return $ showString water
  where 
    msg i = "A parsing problem: the expression parser has \n" ++
            "recognized " ++ show i ++ " more plugs in the source" ++
            "file than the source parser."


evalHoas :: Monad m => Hoas -> NotateT m [ODoc]
evalHoas (Hoas exprs) = foldM eval [] exprs >>= return . reverse
                           

eval :: Monad m => [ODoc] -> HoasExpr -> NotateT m [ODoc]
eval docs (HLet update e)   = local update (eval docs e)
eval docs (HDo out)         = outputNotes out >>= \d  -> return (d:docs)
eval docs (HSDo out e)      = outputNotes out >>= \d  -> eval (d:docs) e
eval docs (HFork e1 e2)     = eval docs e1    >>= \ds -> eval ds e2  
    

outputNotes :: Monad m => OutputDirective -> NotateT m ODoc
outputNotes (OutputDirective (Just OutputRelative) name) = 
    findEventList name >>= maybe (outputFailure name) outputRelativeNoteList 


outputNotes (OutputDirective Nothing name)  = 
    asks output_format >>= \fmt -> 
    case fmt of
      Abc -> findEventList name >>= 
             maybe (outputFailure name) outputNoteListAbc 
      Ly  -> findEventList name >>= 
             maybe (outputFailure name) outputAbsoluteNoteList



findEventList :: Monad m => String -> NotateT m (Maybe EventList)
findEventList name = asks_config _system >>= \sys ->
                     return $ Map.lookup name sys


outputFailure :: Monad m => String -> NotateT m ODoc
outputFailure name = 
    textoutput 0 "ERROR - 'outputNotes'" ("missing " ++ name)   >> 
    asks score_comment                                          >>= \fn -> 
    return $ fn $ "HNOTATE - output failure, missing " ++ name

-- Only option for Abc
outputNoteListAbc :: Monad m => EventList -> NotateT m ODoc
outputNoteListAbc = 
    toNoteList >=> translateAbc abcConcat


outputRelativeNoteList :: Monad m => EventList -> NotateT m ODoc
outputRelativeNoteList = 
    toNoteList >=> translateLilyPond lyConcat lilypondRelativeForm 
    
outputAbsoluteNoteList :: Monad m => EventList -> NotateT m ODoc
outputAbsoluteNoteList evts = 
    (textoutput 3 "Lilypond 'absolute'" "")   >> 
    toNoteList evts >>= translateLilyPond lyConcat lilypondAbsoluteForm 
    

                                