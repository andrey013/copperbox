
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
import HNotate.DebugUtils (printTextChunks)
import HNotate.Env
import HNotate.Monads
import HNotate.MusicRepDatatypes
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


data Config = Config { _system :: System }

type OutputM a = OutputReaderM Env Config a
type OutputDebugM a = OutputReaderDebugM Env Config a



-- {NOTE} Some elements in the env have defaults
-- that might be too arbitrary (e.g. meter pattern)

outputLilyPond :: System -> FilePath -> FilePath -> IO ()
outputLilyPond sys = 
    generalOutput default_ly_env sys lyExprView_TwoPass lyTextChunks
        
outputLilyPond_debug :: System -> FilePath -> FilePath -> IO ()
outputLilyPond_debug sys inpath outpath =
    putStrLn "Generating LilyPond..." >> 
    generalOutput_debug default_ly_env 
                        sys 
                        lyExprView_TwoPass_debug 
                        lyTextChunks
                        inpath
                        outpath                               


outputAbc :: System -> FilePath -> FilePath -> IO ()
outputAbc sys = 
    generalOutput default_abc_env sys abcExprView_TwoPass abcTextChunks

outputAbc_debug :: System -> FilePath -> FilePath -> IO ()
outputAbc_debug sys inpath outpath = 
    putStrLn "Generating Abc..." >>
    generalOutput_debug default_abc_env 
                        sys
                        abcExprView_TwoPass_debug 
                        abcTextChunks
                        inpath
                        outpath
    


generalOutput :: Env -> System -> ExprParser -> TextChunkParser 
              -> FilePath -> FilePath -> IO ()
generalOutput env sys view_parser chunk_parser infile outfile =
    prodM (eitherErrFailIO . view_parser) 
          (eitherErrFailIO . parseFromFile chunk_parser)
          (dup infile)    
      >>= writeS outfile . uncurry (outputter env sys)
    


generalOutput_debug :: Env -> System -> ExprParser -> TextChunkParser 
              -> FilePath -> FilePath -> IO ()
generalOutput_debug  env sys view_parser chunk_parser infile outfile =
    prodM (eitherErrFailIO_debug "Expression Representation:" 
                                 (vsep . map pretty)
                  . view_parser) 
          (eitherErrFailIO_debug "Text Representation:" 
                                 (string . printTextChunks)
                  . parseFromFile chunk_parser)
          (dup infile)    
      >>= writeS outfile . uncurry (outputter env sys)
      

writeS :: FilePath -> ShowS -> IO ()
writeS path = writeFile path . ($ "")                                      

outputter :: Env -> System -> [Expr] -> Seq TextChunk -> ShowS
outputter env sys exprs chunks = plug chunks $ 
    (runOutputReader `flipper` (Config sys) $ env) (evalHoas $ toHoas exprs)
                                     

eitherWithErrIO :: Show a => IO (Either a b) -> (b -> IO c) ->  IO c
eitherWithErrIO a sk = a >>= either (error . show) sk 

eitherErrFailIO :: Show a => IO (Either a b) ->  IO b
eitherErrFailIO a = a >>= either (error . show) return 

eitherErrFailIO_debug :: (Show a, Show b) => 
                      String -> (b -> Doc) -> IO (Either a b) -> IO b
eitherErrFailIO_debug msg pp a = 
    runIOInIO (genWriteStepM msg pp eitherErrFailIO a)



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
     

evalHoas :: Hoas -> OutputM [Doc]
evalHoas (Hoas exprs) = foldM eval [] exprs >>= return . reverse
                           

eval :: [Doc] -> HoasExpr -> OutputM [Doc]
eval docs (HLetExpr update xs)        = 
    foldM (\ds e -> local update (eval ds e)) docs xs

eval docs (HOutputDirective oscm name) = 
    outputNotes (fromMaybe OutputDefault oscm) name >>= return . flip (:) docs

outputNotes :: OutputScheme -> String 
            -> OutputM NoteListOutput
outputNotes OutputRelative name = 
    ask                       >>= \env ->
    asksConfig _system        >>= \sys -> 
    maybe fault (noteListOutput env) (Map.lookup name sys)
  where 
    fault        = error $ "output failure - missing " ++ name


outputNotes OutputDefault  name = outputNotes OutputRelative name 

noteListOutput :: Env -> EventList -> OutputM NoteListOutput
noteListOutput env = 
  abcly env (return . translateAbc env . toNoteList env) 
            (return . translateLilyPond env . toNoteList env)

    
    
