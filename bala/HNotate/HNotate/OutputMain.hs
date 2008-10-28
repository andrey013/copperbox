
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
import HNotate.Document (ODoc, formatted, output)
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (ExprParser, TextChunkParser)
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
-- import Text.PrettyPrint.Leijen hiding ( (<$>) ) 







-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: Int -> System -> FilePath -> FilePath -> IO ()
outputLilyPond dl sys inpath outpath   = do 
    (a,msg) <- runNotateT outfun default_ly_env config 
    putStrLn msg
  where
    config  = mkLyConfig dl sys inpath outpath
    outfun  = generalOutput lyExprParse lyTextChunks 
    
outputAbc :: Int -> System -> FilePath -> FilePath -> IO ()
outputAbc dl sys inpath outpath        = do 
    (a,msg) <- runNotateT outfun default_abc_env config  
    putStrLn msg
  where
    config  = mkAbcConfig dl sys inpath outpath
    outfun  = generalOutput abcExprParse abcTextChunks
    



generalOutput :: ExprParser -> TextChunkParser -> NotateT IO ()
generalOutput expr_parser chunk_parser  = do 
    infile    <- asks_config _template_file
    cks       <- liftIO $ parseFromFile chunk_parser infile
    either (fault chunk_fail_msg) (step2 infile) cks
  where
    chunk_fail_msg = "Failure running the 'chunk' parser..."
    expr_fail_msg  = "Failure running the 'expression' parser..." 
    
    fault msg err = textoutput 0 msg (show err) >> return ()
    
    step2 infile chks  =  do 
        exprs <- expr_parser infile
        either (fault expr_fail_msg) (step3 chks) exprs
    
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



plug :: Seq TextChunk -> [ODoc] -> ShowS
plug se ds = foldr fn id (crossZip se ds)
  where 
    fn ((water, _), Just d)  acc = showString water . output 0 60 d . acc
    fn ((water, _), Nothing) acc = showString water . acc

-- Expect the Seq to be one longer than the list   
crossZip :: Seq a -> [b] -> [(a,Maybe b)]
crossZip se xs = czip (viewl se) xs
  where
    czip (e :< se) (x:xs) = (e,Just x)  : czip (viewl se) xs
    czip (e :< se) []     = (e,Nothing) : czip (viewl se) []
    
    czip EmptyL    []     = []
    
    czip _         xs     = error $ "crossZip - list too long"  
     

evalHoas :: Monad m => Hoas -> NotateT m [ODoc]
evalHoas (Hoas exprs) = foldM eval [] exprs >>= return . reverse
                           

eval :: Monad m => [ODoc] -> HoasExpr -> NotateT m [ODoc]
eval docs (HLet update e)   = local update (eval docs e)
eval docs (HDo out)         = outputNotes out >>= \d -> return (d:docs)
eval docs (HSDo out e)      = outputNotes out >>= \d -> eval (d:docs) e
eval docs (HFork e1 e2)     = eval docs e1 >>= \ds -> eval ds e2  
    

outputNotes :: Monad m => OutputDirective -> NotateT m ODoc
outputNotes (OutputDirective (Just OutputRelative) name) = 
    maybe fault noteListOutput =<< findEventList name
  where 
    fault        = error $ "output failure - missing " ++ name

outputNotes (OutputDirective Nothing name)  = 
    outputNotes (OutputDirective (Just OutputRelative) name) --- ARRGH!! 


findEventList :: Monad m => String -> NotateT m (Maybe EventList)
findEventList name = asks_config _system >>= \sys ->
                     return $ Map.lookup name sys




noteListOutput :: Monad m => EventList -> NotateT m ODoc
noteListOutput = 
    abcly (toNoteList >=> translateAbc abcConcat) 
          (toNoteList >=> translateLilyPond lyConcat) 


    
