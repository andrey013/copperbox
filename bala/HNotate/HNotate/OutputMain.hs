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
import HNotate.CommonUtils
import HNotate.BuildNoteList
import HNotate.Document ( ODoc, ODocS, emptyDoc, output, formatted )
import HNotate.Env
import HNotate.FPList hiding (length)
import qualified HNotate.FPList as Fpl
import HNotate.MiniMidi ( MidiFile(..), MidiTrack (..), writeMidiFile )
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (ExprParser, TextSourceParser)
import HNotate.ProcessingBase
import HNotate.TemplateDatatypes

import Control.Applicative hiding (empty)
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer


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

outputMidi :: (Env -> Env) -> NotateT IO [EventList] 
                      -> System -> FilePath -> IO ()
outputMidi f ma sys outpath = 
    runNotateT outfun (f default_midi_env) config         >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkMidiConfig 5 sys outpath
    outfun  = ma >>= mapM buildNoteList >>= midiOut outpath

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

-- Plugging a parsed doc is nice and simple. 
-- Particularly, any spliced nesting will be captured in the water
-- on either side of the island. 
-- (Plugging a handbuilt document is more complicated).
plugParsedTemplate :: ParsedTemplate -> [ODoc] -> String
plugParsedTemplate fpls ys = 
    concatNoSpace $ merge showsWater showsIsland $ knitOnB (,) ys fpls
  where
    showsIsland :: (SrcLoc,ODoc) -> ShowS
    showsIsland (loc,doc) = output (_src_column loc) 80 doc
    
    showsWater :: String -> ShowS
    showsWater = showString 
    


outputLilyPondDocu :: Int -> System -> HandBuiltLilyPond -> FilePath -> IO ()
outputLilyPondDocu dl sys (HBLP docuh) outpath   =
    runNotateT outfun default_ly_env config           >>= \(a,msg) ->
    either (reportFailureIO msg) (const $ putStrLn msg) a
  where
    config  = mkLyConfig dl sys "" outpath
    outfun  = docuOutput docuh

outputAbcDocu :: Int -> System -> HandBuiltAbc -> FilePath -> IO ()
outputAbcDocu dl sys (HBAbc docuh) outpath   =
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


-- Docs created by hand must be able to generate the appropriate 
-- nesting in output. This is achieved by having /higher-order water/
-- in the FPList - the water is an ODocS function to fit the island.
-- So we need to pass the island to the water with a bimerge.     
plugHandDoc :: FPList ODocS a -> [ODoc] -> String
plugHandDoc fpls ys = 
    concatNoSpace $ bimerge pairwise flush $ knitOnB swap ys fpls
  where
    pairwise :: ODocS -> ODoc -> ShowS
    pairwise f d = output 0 80 (f d)
    
    flush :: ODocS -> ShowS
    flush f = output 0 80 (f emptyDoc)
    
    swap :: a -> b -> b
    swap _ = id
       
        
{-

-}

writeODocToFile :: FilePath -> ODoc -> IO ()
writeODocToFile path odoc  = writeFile path (formatted 0 70 odoc)


evalHoas :: Monad m => Hoas -> NotateT m [ODoc]
evalHoas (Hoas exprs) = reverse <$> foldM evalHoasStep [] exprs 

evalPartialHoas :: Monad m => [Maybe HoasExpr] -> NotateT m [ODoc]
evalPartialHoas oexprs = reverse <$> foldM fn [] oexprs where
    fn docs Nothing   = return $ emptyDoc:docs
    fn docs (Just e)  = evalHoasStep docs e


 
evalHoasStep :: Monad m => [ODoc] -> HoasExpr -> NotateT m [ODoc]
evalHoasStep docs (HLet update e)    = local update (evalHoasStep docs e)

evalHoasStep docs (HDo out)          = (\d -> d:docs) <$> outputNotes out

evalHoasStep docs (HDoExpr out e)    = do { d <- outputNotes out
                                          ; evalHoasStep (d:docs) e }
                                      
evalHoasStep docs (HFork e1 e2)      = do { docs' <- evalHoasStep docs e1
                                          ; evalHoasStep docs' e2}  
 
    


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
 

midiScoreProcessor :: ScoreProcessor IO template MidiTrack MidiFile
midiScoreProcessor = ScoreProcessor { 
    reformulate_notelist      = \_ nl -> return nl,
    notelist_to_target        = translateMidi,
    assemble_target_fragments = undefined,
    output_to_file            = writeMidiFile
  }

-}                    