
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
import HNotate.CommonUtils
import HNotate.Document ( ODoc, ODocS, emptyDoc, output, formatted, 
                          ( <+> ) , ( <&\> ), dblvsep )
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
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkLyConfig dl sys inpath outpath
    outfun  = generalOutput lyExprParse lyTextSource 
    
outputAbc :: Int -> System -> FilePath -> FilePath -> IO ()
outputAbc dl sys inpath outpath        =
    runNotateT outfun default_abc_env config          >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkAbcConfig dl sys inpath outpath
    outfun  = generalOutput abcExprParse abcTextSource

outputMidi :: (Env -> Env) -> NotateT IO [EventList] 
                      -> System -> FilePath -> IO ()
outputMidi f ma sys outpath = 
    runNotateT outfun (f default_midi_env) config         >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkMidiConfig 5 sys outpath
    outfun  = ma >>= mapM toNoteList >>= midiOut outpath

reportFailureIO :: String -> NotateErr -> IO ()
reportFailureIO log_msg (NotateErr s) = putStrLn s >> putStrLn log_msg

-- 3 schemes that must be exported
getEventList :: String -> NotateT IO [EventList]
getEventList name = getEventList1 name >>= \a -> return [a]

namedEventLists :: [String] -> NotateT IO [EventList]
namedEventLists names = mapM getEventList1 names 

allEventLists :: NotateT IO [EventList]
allEventLists = asks_config _system >>= \sys ->
                return $ Map.elems sys


getEventList1 :: String -> NotateT IO EventList
getEventList1 name = findEventList name >>= maybe fk sk
  where
    fk = throwError (strMsg $ "Could not find " ++ name)
    sk a = return a 


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

outputLilyPondDocu :: Int -> System -> DocuHoas -> FilePath -> IO ()
outputLilyPondDocu dl sys docuh outpath   =
    runNotateT outfun default_ly_env config           >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkLyConfig dl sys "" outpath
    outfun  = docuOutput docuh
    

outputAbcDocu :: Int -> System -> DocuHoas -> FilePath -> IO ()
outputAbcDocu dl sys docuh outpath   =
    runNotateT outfun default_abc_env config           >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkAbcConfig dl sys "" outpath
    outfun  = docuOutput docuh
    
        
docuOutput :: DocuHoas -> NotateT IO ()
docuOutput dhoas = do
    out   <- asks_config _output_file
    docf  <- evalDocuHoas dhoas
    liftIO $ writeFile out (formatted 0 70 (dblvsep $ fmap ($ emptyDoc) docf))

evalHoas :: Monad m => Hoas -> NotateT m [ODoc]
evalHoas (Hoas exprs) = foldM eval [] exprs >>= return . reverse
                           
-- Standard evaluation - used when plugging holes in template files 
eval :: Monad m => [ODoc] -> HoasExpr () -> NotateT m [ODoc]
eval docs (HLet update _ e)   = local update (eval docs e)
eval docs (HDo out)           = outputNotes out >>= \d  -> return (d:docs)
eval docs (HSDo out e)        = outputNotes out >>= \d  -> eval (d:docs) e
eval docs (HFork e1 e2)       = eval docs e1    >>= \ds -> eval ds e2  
eval docs (HText _ e)         = eval docs e -- should not occur in standard eval   
eval docs (HText0 d)          = return [] -- should not occur in standard eval 


evalDocuHoas :: Monad m => DocuHoas -> NotateT m [ODocS]
evalDocuHoas (Hoas exprs) = mapM (docuEval id) exprs

-- /Document evaluation/ - used when creating output from ODoc combinators 
docuEval :: Monad m => ODocS -> HoasExpr ODocS -> NotateT m ODocS
docuEval f (HLet update d e)  = local update (docuEval (f . d) e)

docuEval f (HDo out)          = outputNotes out >>= \d -> 
                                return (f . (<&\> d))

docuEval f (HSDo out e)       = outputNotes out >>= \d -> 
                                docuEval (f . (<&\> d)) e

docuEval f (HFork e1 e2)      = docuEval f e1 >>= \f' -> docuEval f' e2  

docuEval f (HText d e )       = docuEval (f . d) e 

docuEval f (HText0 d)         = return (f . d) 


outputNotes :: Monad m => OutputDirective -> NotateT m ODoc
outputNotes (OutputDirective (Just OutputRelative) name) = 
    findEventList name >>= maybe (outputFailure name) outputRelativeNoteList 

outputNotes (OutputDirective (Just OutputDefault) name) = 
    outputNotes (OutputDirective Nothing name) 
    
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
    

                                