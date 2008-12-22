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
import HNotate.CommonUtils (singleQuote)
import HNotate.BuildNoteList
import HNotate.Document ( ODoc, emptyDoc  )
import HNotate.Env
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

import Data.Foldable (foldrM)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec (parseFromFile)


noAnnoEval :: AnnoEval
noAnnoEval = AnnoEval (\_ _ -> id) (\_ _ -> id) (\_ _ -> id) 

-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: Int -> System -> FilePath -> FilePath -> IO ()
outputLilyPond dl sys inp outp = outputLilyPond' dl sys noAnnoEval inp outp

outputLilyPond' :: Int -> System -> AnnoEval -> FilePath -> FilePath -> IO ()
outputLilyPond' dl sys aeval inp outp   =
    runMain (filebasedOutput lyExprParse lyTextSource) 
            (makeLyEnv dl) 
            (makeLyConfig sys aeval inp outp)
            
outputAbc :: Int -> System -> FilePath -> FilePath -> IO ()
outputAbc dl sys inp outp = outputAbc' dl sys noAnnoEval inp outp

              
outputAbc' :: Int -> System -> AnnoEval -> FilePath -> FilePath -> IO ()
outputAbc' dl sys aeval inp outp =
    runMain (filebasedOutput abcExprParse abcTextSource) 
            (makeAbcEnv dl) 
            (makeAbcConfig sys aeval inp outp)
 
runMain :: (NotateT IO ()) -> Env -> Config -> IO ()
runMain mf env cfg = runNotateT mf env cfg >>= fn where
    fn (Left err,logg) = reportFailureIO logg err
    fn (Right _, logg)  = putStrLn logg

reportFailureIO :: String -> NotateErr -> IO ()
reportFailureIO log_msg (NotateErr err) = 
    mapM_ putStrLn [log_msg,"",dashes,"",err] where dashes = replicate 60 '-' 


proceedRight :: (Monad m, Show a) => 
    String -> NotateT m (Either a b) -> NotateT m b
proceedRight err_msg mf = mf >>= next where
  next (Left a)   = throwError $ strMsg $ err_msg ++ "\n" ++ show a
  next (Right b)  = return b   

parseTemplate :: TextSourceParser -> FilePath -> NotateT IO ParsedTemplate
parseTemplate src_parser path  = 
    proceedRight msg $ liftIO $ parseFromFile src_parser path
  where
    msg = "Failure running the 'template' parser..."
    
parseExpressions :: ExprParser -> FilePath -> NotateT IO [Expr]
parseExpressions expr_parser path = 
    proceedRight msg $ expr_parser path
  where 
    msg = "Failure running the 'expression' parser..."
  

filebasedFrontend :: FilePath -> TextSourceParser -> ExprParser  
    -> NotateT IO (ParsedTemplate,[Expr])
filebasedFrontend path src_parser expr_parser = 
    (,) <$> parseTemplate src_parser path <*> parseExpressions expr_parser path


filebasedOutput :: ExprParser -> TextSourceParser -> NotateT IO ()
filebasedOutput expr_parser src_parser  = do 
    infile      <- asks_config _template_file
    (src,exprs) <- filebasedFrontend infile src_parser expr_parser
    out         <- asks_config _output_file 
    odocs       <- evalHoas (toHoas exprs)
    liftIO $ writeFile out (plugParsedTemplate src odocs)
                                  



outputLilyPondDocu :: Int -> System -> LilyPondTemplate -> FilePath -> IO ()
outputLilyPondDocu dl sys template outpath   =
    outputLilyPondDocu' dl sys noAnnoEval template outpath

outputLilyPondDocu' :: Int -> System -> AnnoEval -> LilyPondTemplate -> FilePath -> IO ()
outputLilyPondDocu' dl sys aeval (LyTemplate docuh) outpath =
    runMain (docuOutput docuh) (makeLyEnv dl) lycfg 
  where
    lycfg  = makeLyConfig sys aeval "" outpath
 


outputAbcDocu :: Int -> System -> AbcTemplate -> FilePath -> IO ()
outputAbcDocu dl sys template outpath   =
    outputAbcDocu' dl sys noAnnoEval template outpath
    
outputAbcDocu' :: Int -> System -> AnnoEval -> AbcTemplate -> FilePath -> IO ()
outputAbcDocu' dl sys aeval (AbcTemplate docuh) outpath =
    runMain (docuOutput docuh) (makeAbcEnv dl) abccfg
  where
    abccfg = makeAbcConfig sys aeval "" outpath    
    

docuOutput :: (HandBuiltTemplate, [Maybe HoasExpr]) -> NotateT IO ()
docuOutput (pdoc,ohoas) = do
    out     <- asks_config _output_file
    odocs   <- evalPartialHoas ohoas
    liftIO $ writeFile out (plugHandDoc pdoc odocs)



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

outputNotes :: Monad m => OutputDirective -> NotateT m ODoc
outputNotes (OutputDirective d name) = do 
    fmt   <- asks output_format
    evts  <- findEventList name
    notes <- buildNoteList evts
    output fmt d notes
  where
    output :: Monad m => 
        OutputFormat -> Maybe OutputScheme -> NoteList -> NotateT m ODoc
    output OutputAbc  _                      = outputNoteListAbc 
    output OutputLy   (Just OutputRelative)  = outputRelativeNoteList
    output OutputLy   _                      = outputAbsoluteNoteList 


findEventList :: Monad m => String -> NotateT m EventList
findEventList name = do
    System sys <- asks_config _system
    maybe failure return (Map.lookup name sys)
  where
    failure =  throwError $ strMsg $ (singleQuote name) ++ 
                  " missing in the system. No output will be generated."
   
-- Only option for Abc
outputNoteListAbc :: Monad m => NoteList -> NotateT m ODoc
outputNoteListAbc = translateAbc abcConcat


outputRelativeNoteList :: Monad m => NoteList -> NotateT m ODoc
outputRelativeNoteList = 
    lilypondRelativeForm >=> translateLilyPond lyConcat 

    
outputAbsoluteNoteList :: Monad m => NoteList -> NotateT m ODoc
outputAbsoluteNoteList = witness 3 "Lilypond 'absolute'" >=> fn where
  fn = lilypondAbsoluteForm >=> translateLilyPond lyConcat  
   

                 