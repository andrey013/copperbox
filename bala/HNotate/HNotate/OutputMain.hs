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


data DebugLevel = DebugOn | DebugOff
  deriving (Eq,Show)

-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: DebugLevel -> System -> FilePath -> FilePath -> IO ()
outputLilyPond dl sys inp outp   =
    runMain dl (filebasedOutput lyExprParse lyTextSource) 
               default_ly_env 
               (makeLyConfig sys inp outp)
            
             
outputAbc :: DebugLevel -> System -> FilePath -> FilePath -> IO ()
outputAbc dl sys inp outp =
    runMain dl (filebasedOutput abcExprParse abcTextSource) 
               default_abc_env 
               (makeAbcConfig sys inp outp)
 
runMain :: DebugLevel -> (NotateT IO ()) -> Env -> Config -> IO ()
runMain dl mf env cfg = runNotateT mf env cfg >>= fn where
    fn (Left err,logg)  = reportFailureIO logg err
    fn (Right _, logg)  = if (dl==DebugOn) then putStrLn logg 
                                           else putStrLn "Done."
                             

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
                                  



outputLilyPondDocu :: DebugLevel -> System -> LilyPondTemplate -> FilePath -> IO ()
outputLilyPondDocu dl sys (LyTemplate docuh) outpath =
    runMain dl (docuOutput docuh) default_ly_env lycfg 
  where
    lycfg  = makeLyConfig sys "" outpath
 


outputAbcDocu :: DebugLevel -> System -> AbcTemplate -> FilePath -> IO ()
outputAbcDocu dl sys (AbcTemplate docuh) outpath =
    runMain dl (docuOutput docuh) default_abc_env abccfg
  where
    abccfg = makeAbcConfig sys "" outpath    
    

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
    fmt             <- asks output_format
    (evts,aeval)    <- findEventList name
    notes           <- buildNoteList evts
    output fmt d aeval notes
  where
    output :: Monad m => OutputFormat -> Maybe OutputScheme -> AnnoEval 
                                      -> NoteList -> NotateT m ODoc
    output OutputAbc  _                      = outputNoteListAbc 
    output OutputLy   (Just OutputRelative)  = outputRelativeNoteList
    output OutputLy   _                      = outputAbsoluteNoteList 


findEventList :: Monad m => String -> NotateT m (EventList,AnnoEval)
findEventList name = do
    System sys <- asks_config _system
    maybe failure return (Map.lookup name sys)
  where
    failure =  throwError $ strMsg $ (singleQuote name) ++ 
                  " missing in the system. No output will be generated."
   
-- Only option for Abc
outputNoteListAbc :: Monad m => AnnoEval -> NoteList -> NotateT m ODoc
outputNoteListAbc = translateAbc abcConcat


outputRelativeNoteList :: Monad m => AnnoEval -> NoteList -> NotateT m ODoc
outputRelativeNoteList aeval = 
    lilypondRelativeForm >=> translateLilyPond lyConcat aeval

    
outputAbsoluteNoteList :: Monad m => AnnoEval -> NoteList -> NotateT m ODoc
outputAbsoluteNoteList aeval = witness "Lilypond 'absolute'" >=> fn where
  fn = lilypondAbsoluteForm >=> translateLilyPond lyConcat aeval  
   

                 