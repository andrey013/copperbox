
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
import HNotate.NoteListDatatypes
import HNotate.ParseAbc
import HNotate.ParseLy
import HNotate.ParserBase (ExprParser, TextChunkParser)
import HNotate.TemplateDatatypes

import Control.Applicative hiding (empty)
import Control.Monad.Reader

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Sequence hiding (empty)

import Text.ParserCombinators.Parsec (ParseError, parseFromFile)
import Text.PrettyPrint.Leijen hiding ( (<$>) ) 


data Config = Config { _system :: System }

type OutputReaderM a = ReaderT Env (Reader Config) a
  
runOutputReader :: OutputReaderM a -> Config -> Env -> a
runOutputReader f config env = runReader (runReaderT f env) config



-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: System -> FilePath -> FilePath -> IO ()
outputLilyPond sys = 
    generalOutput default_ly_env sys lyExprView_TwoPass lyTextChunks
        
                                 


outputAbc :: System -> FilePath -> FilePath -> IO ()
outputAbc sys = 
    generalOutput default_abc_env sys abcExprView_TwoPass abcTextChunks



generalOutput :: Env -> System -> ExprParser -> TextChunkParser 
              -> FilePath -> FilePath -> IO ()
generalOutput env sys view_parser chunk_parser infile outfile =
    prodM (eitherErrFailIO . view_parser) 
          (eitherErrFailIO . parseFromFile chunk_parser)
          (dup infile)    
      >>= writeS outfile . uncurry (outputter env sys)
    

writeS :: FilePath -> ShowS -> IO ()
writeS path = writeFile path . ($ "")                                      

outputter :: Env -> System -> [Expr] -> Seq TextChunk -> ShowS
outputter env sys exprs chunks = plug chunks $ 
    runOutputReader (evalHoas $ toHoas exprs) (Config sys) env  
                                      

                                      
eitherWithErrIO :: Show a => IO (Either a b) -> (b -> IO c) ->  IO c
eitherWithErrIO a sk = a >>= either (error . show) sk 

eitherErrFailIO :: Show a => IO (Either a b) ->  IO b
eitherErrFailIO a = a >>= either (error . show) return 





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
     


evalHoas :: Hoas -> OutputReaderM [Doc]
evalHoas (Hoas exprs) = foldM eval [] exprs

eval :: [Doc] -> HoasExpr -> OutputReaderM [Doc]
eval docs (HLetExpr update xs)        = 
    foldM (\ds e -> local update (eval ds e)) docs xs

eval docs (HOutputDirective oscm name) = 
    output (fromMaybe OutputDefault oscm) name >>= return . flip (:) docs


output OutputRelative name = do
  sys       <- lift $ asks _system
  current_env   <- ask
  case (Map.lookup name sys) of
      Nothing       -> error $ "output failure - missing " ++ name
      Just evtlist  -> case output_format current_env of
                          Output_LilyPond -> 
                              return $ translateLilyPond (toNoteList evtlist current_env) current_env
                          Output_Abc      -> 
                              return $ translateAbc (toNoteList evtlist current_env) current_env
  

  
  
output OutputDefault  name = output OutputRelative name 

