
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
import HNotate.CommonUtils -- (outputDoc, showDocS)
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (ExprParser, TextChunkParser)
import HNotate.PrintMonad (NoteListOutput)
import HNotate.TemplateDatatypes

import Control.Applicative hiding (empty)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence hiding (empty, reverse)

import Text.ParserCombinators.Parsec (ParseError, parseFromFile)
import Text.PrettyPrint.Leijen hiding ( (<$>) ) 







-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: System -> FilePath -> FilePath -> IO ()
outputLilyPond sys inpath outpath   = do 
    (a,msg) <- runNotateT (generalOutput lyparsers) default_ly_env config 
    putStrLn msg
  where
    config      = mkLyConfig sys inpath outpath
    lyparsers   = (lyExprView_TwoPass, lyTextChunks) 
    
outputAbc :: System -> FilePath -> FilePath -> IO ()
outputAbc sys inpath outpath        = do 
    (a,msg) <- runNotateT (generalOutput abcparsers) default_abc_env config  
    putStrLn msg
  where
    config      = mkAbcConfig sys inpath outpath
    abcparsers  = (abcExprView_TwoPass, abcTextChunks) 
    



generalOutput :: (ExprParser, TextChunkParser) -> NotateT IO ()
generalOutput (expr_parser, chunk_parser)  = do 
    infile    <- asks_config _template_file
    cks       <- liftIO $ parseFromFile chunk_parser infile
    either fault (step2 infile) cks
  where
    fault err = error $ show err
    step2 infile chks  =  do 
        exprs <- expr_parser infile
        either fault (step3 chks) exprs
    
    step3 :: Seq TextChunk -> [Expr] ->  NotateT IO ()
    step3 chunks exprs = do
        out   <- asks_config _output_file 
        fn    <- outputter exprs chunks
        liftIO $ writeS out fn

      

writeS :: FilePath -> ShowS -> IO ()
writeS path = writeFile path . ($ "")                                      

outputter :: Monad m => [Expr] -> Seq TextChunk -> NotateT m ShowS
outputter exprs chunks = evalHoas (toHoas exprs) >>= \exprs' ->
                         return (plug chunks exprs')

                                     

eitherWithErrIO :: Show a => IO (Either a b) -> (b -> IO c) ->  IO c
eitherWithErrIO a sk = a >>= either (error . show) sk 

eitherErrFailIO :: Show a => IO (Either a b) ->  IO b
eitherErrFailIO a = a >>= either (error . show) return 

eitherNT :: (Show a, Show b) => 
                      String -> (b -> Doc) -> IO (Either a b) -> IO b
eitherNT msg pp a = eitherErrFailIO a 
--    runIOInIO (genWriteStepM msg pp eitherErrFailIO a)



plug :: Seq TextChunk -> [Doc] -> ShowS
plug se ds = foldr fn id (crossZip se ds)
  where 
    fn ((water, _), Just d)  acc = showString water . showDocS d . acc
    fn ((water, _), Nothing) acc = showString water . acc

-- Expect the Seq to be one longer than the list   
crossZip :: Seq a -> [b] -> [(a,Maybe b)]
crossZip se xs = czip (viewl se) xs
  where
    czip (e :< se) (x:xs) = (e,Just x) : czip (viewl se) xs
    czip (e :< se) []     = (e,Nothing) : czip (viewl se) []
    
    czip EmptyL    []     = []
    
    czip _         xs     = error $ "crossZip - list too long"  
     

evalHoas :: Monad m => Hoas -> NotateT m [Doc]
evalHoas (Hoas exprs) = foldM eval [] exprs >>= return . reverse
                           

eval :: Monad m => [Doc] -> HoasExpr -> NotateT m [Doc]
eval docs (HLetExpr update xs)        = 
    foldM (\ds e -> local update (eval ds e)) docs xs

eval docs (HOutputDirective oscm name) = 
    outputNotes (fromMaybe OutputDefault oscm) name >>= return . flip (:) docs

outputNotes :: Monad m => OutputScheme -> String 
            -> NotateT m NoteListOutput
outputNotes OutputRelative name = 
    maybe fault noteListOutput =<< findEventList name
  where 
    fault        = error $ "output failure - missing " ++ name

outputNotes OutputDefault  name = outputNotes OutputRelative name 


findEventList :: Monad m => String -> NotateT m (Maybe EventList)
findEventList name = asks_config _system >>= \sys ->
                     return $ Map.lookup name sys




noteListOutput :: Monad m => EventList -> NotateT m NoteListOutput
noteListOutput = 
  abcly (toNoteList >=> translateAbc) (toNoteList >=> translateLilyPond)

    
    
