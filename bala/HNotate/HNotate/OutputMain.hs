{-# OPTIONS -Wall #-}

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
import HNotate.DocMidi
import HNotate.Document ( ODoc, emptyDoc  )
import HNotate.Env
import HNotate.MiniMidi
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (ExprParser, TextSourceParser)
import HNotate.ProcessingBase
import HNotate.TemplateDatatypes

import Control.Applicative hiding (empty)
import Data.Sequence hiding (update)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Foldable (foldrM)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec (parseFromFile)


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
    
    step2 :: FilePath -> ParsedTemplate -> NotateT IO ()
    step2 infile src  =  do 
        exprs <- expr_parser infile
        either (fault expr_fail_msg) (step3 src) exprs
    
    step3 :: ParsedTemplate -> [Expr] -> NotateT IO ()
    step3 src exprs = do
        out   <- asks_config _output_file 
        txt   <- outputter exprs src
        liftIO $ writeFile out txt
                                    

outputter :: Monad m => [Expr] -> ParsedTemplate -> NotateT m String
outputter exprs src = do 
    odocs <- evalHoas (toHoas exprs)
    return $ plugParsedTemplate src odocs


    
midiOut :: FilePath -> [NoteList] -> NotateT IO ()
midiOut _ _ = undefined

outputLilyPondDocu :: Int -> System -> LilyPondTemplate -> FilePath -> IO ()
outputLilyPondDocu dl sys (LyTemplate docuh) outpath   =
    runNotateT outfun default_ly_env config           >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkLyConfig dl sys "" outpath
    outfun  = docuOutput docuh

outputAbcDocu :: Int -> System -> AbcTemplate -> FilePath -> IO ()
outputAbcDocu dl sys (AbcTemplate docuh) outpath   =
    runNotateT outfun default_abc_env config           >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkAbcConfig dl sys "" outpath
    outfun  = docuOutput docuh
    
    

docuOutput :: (HandBuiltTemplate, [Maybe HoasExpr]) -> NotateT IO ()
docuOutput (pdoc,ohoas) = do
    out     <- asks_config _output_file
    odocs   <- evalPartialHoas ohoas
    let txt = plugHandDoc pdoc odocs
    liftIO $ writeFile out txt


outputMidi :: Int -> System -> MidiTemplate -> FilePath -> IO ()
outputMidi dl sys (MidiTemplate docuh) outpath = 
    runNotateT outfun default_midi_env config           >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkMidiConfig dl sys outpath
    outfun  = handmidiOutput docuh
    
    
handmidiOutput :: (HandBuiltMidi, [Maybe HoasExpr]) -> NotateT IO () 
handmidiOutput (pdoc,ohoas) = do
    out     <- asks_config _output_file
    notes   <- evalPartialHoasMidi ohoas
    let trks = plugMidiTemplate pdoc (fmap (fmap ExtMidi) notes)
    mf      <- makeMidiFile trks
    liftIO $ writeMidiFile out mf

evalHoas :: Monad m => Hoas -> NotateT m [ODoc]
evalHoas (Hoas exprs) = foldrM evalHoasStep [] exprs 

evalPartialHoas :: Monad m => [Maybe HoasExpr] -> NotateT m [ODoc]
evalPartialHoas oexprs = foldrM fn [] oexprs where
    fn Nothing   docs = return $ emptyDoc:docs
    fn (Just e)  docs = evalHoasStep e docs


 
evalHoasStep :: Monad m => HoasExpr -> [ODoc] -> NotateT m [ODoc]
evalHoasStep (HLet update e)    docs = local update (evalHoasStep e docs)

evalHoasStep (HDo out)          docs = (\d -> d:docs) <$> outputNotes out

evalHoasStep (HDoExpr out e)    docs = do { d <- outputNotes out
                                          ; evalHoasStep e (d:docs) }
                                      
evalHoasStep (HFork e1 e2)      docs = evalHoasStep e1 docs >>= evalHoasStep e2



evalPartialHoasMidi :: Monad m => [Maybe HoasExpr] -> NotateT m [Seq Message]
evalPartialHoasMidi oexprs = foldrM fn [] oexprs where
    fn Nothing   acc = return $ empty:acc
    fn (Just e)  acc = evalHoasMidiStep e acc
    

evalHoasMidiStep :: Monad m => HoasExpr -> [Seq Message] -> NotateT m [Seq Message]
evalHoasMidiStep (HLet update e) xs = local update (evalHoasMidiStep e xs)

evalHoasMidiStep (HDo out)       xs = (\d -> d:xs) <$> outputMidiNotes out

evalHoasMidiStep (HDoExpr out e) xs = do { d <- outputMidiNotes out
                                         ; evalHoasMidiStep e (d:xs) }
                                      
evalHoasMidiStep (HFork e1 e2)   xs = evalHoasMidiStep e1 xs >>= evalHoasMidiStep e2

outputMidiNotes :: Monad m => OutputDirective -> NotateT m (Seq Message)
outputMidiNotes (OutputDirective _ name)  =      
    findEventList name >>= maybe (error "outputMidiNotes") sk
  where
    sk evts = buildNoteList evts >>= translateMidi    


outputNotes :: Monad m => OutputDirective -> NotateT m ODoc
outputNotes (OutputDirective (Just OutputRelative) name) = 
    findEventList name >>= maybe (outputFailure name) outputRelativeNoteList 

outputNotes (OutputDirective (Just OutputDefault) name) = 
    outputNotes (OutputDirective Nothing name) 
    
outputNotes (OutputDirective _ name)  = 
    asks output_format >>= \fmt -> 
    case fmt of
      Abc -> findEventList name >>= 
             maybe (outputFailure name) outputNoteListAbc 
      Ly  -> findEventList name >>= 
             maybe (outputFailure name) outputAbsoluteNoteList
      Midi -> fail "cannot output Midi via an OutputDirective"              



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
    buildNoteList >=> translateAbc abcConcat


outputRelativeNoteList :: Monad m => EventList -> NotateT m ODoc
outputRelativeNoteList evts = 
    buildNoteList evts >>= lilypondRelativeForm >>= translateLilyPond lyConcat 

    
outputAbsoluteNoteList :: Monad m => EventList -> NotateT m ODoc
outputAbsoluteNoteList evts = 
    (textoutput 3 "Lilypond 'absolute'" "")   >> 
    buildNoteList evts >>= lilypondAbsoluteForm >>= translateLilyPond lyConcat  
   

--------------------------------------------------------------------------------
-- Experiment...

{-
textsrcLyProcessor  :: ScoreProcessor IO TextSource ODoc ShowS
textsrcLyProcessor = 
    lyScoreProcessor plug writeShowSToFile

docuLyProcessor     :: ScoreProcessor IO a ODoc ODoc
docuLyProcessor     = lyScoreProcessor undefined writeODocToFile
-}

  
lyScoreProcessor :: 
       (template -> [ODoc] -> NotateT IO target)
    -> (FilePath -> target -> IO ()) 
    -> ScoreProcessor IO template ODoc target
lyScoreProcessor knit_fun output_fun = ScoreProcessor { 
    reformulate_notelist      = reformulate,
    notelist_to_target        = translateLilyPond lyConcat,
    assemble_target_fragments = knit_fun,
    output_to_file            = output_fun
  }
  where
    reformulate OutputRelative  = lilypondRelativeForm    
    reformulate OutputDefault   = lilypondAbsoluteForm 

    
abcScoreProcessor :: (template -> [ODoc] -> NotateT IO target) ->
                     (FilePath -> target -> IO ()) -> 
                     ScoreProcessor IO template ODoc target
abcScoreProcessor knit_fun output_fun = ScoreProcessor { 
    reformulate_notelist      = const abcForm,
    notelist_to_target        = translateAbc abcConcat,
    assemble_target_fragments = knit_fun,
    output_to_file            = output_fun
  }
 
{-
midiScoreProcessor :: ScoreProcessor IO template MidiTrack MidiFile
midiScoreProcessor = ScoreProcessor { 
    reformulate_notelist      = \_ nl -> return nl,
    notelist_to_target        = translateMidi,
    assemble_target_fragments = undefined,
    output_to_file            = writeMidiFile
  }

-}                    