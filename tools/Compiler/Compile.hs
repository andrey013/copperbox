
module Compiler.Compile
  ( runCompile
  ) where

import Compiler.Env
import Compiler.Language
import Compiler.CompilerMonad
import Compiler.ParseAsdl
import Compiler.ProcessOpts
import Compiler.Views.ViewBase

import qualified Base.AsdlConcreteSyn as CS
import Base.Lib

-- (2DO) Do we want to explicitly import from the Gen directory?
import Gen.Asdl2Core

import Ext.UUAG.Lib
import Ext.Caml.Lib
import Ext.Haskell.Lib

import PPrint

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Exit
import System.IO
import System.Time


runCompile :: [Flag] -> [FilePath] -> IO ()
runCompile opts files = do
  ans <- compileSteps
  case ans of 
    (Left errmsg, st) -> compileFailure errmsg st
    (Right _,st) -> postmortem st
  where compileSteps = runCompilerM $ do
          processCmdLineOpts opts files
          spec <- processAsdlFiles files
          further generateOutput spec
          


compileFailure :: [Char] -> Compiler.CompilerMonad.CState -> IO ()
compileFailure err st = do 
  postmortem st
  putStrLn $ "Processing Failed: " ++ err
  exitFailure
                   
                   
--------------------------------------------------------------------------------
-- Step 1 
--
-- Process the command line options and build the __ environment.
-- All the work done in the ProcessOpts module
--------------------------------------------------------------------------------                     
processCmdLineOpts :: [Flag] -> [FilePath] -> CompilerM ()
processCmdLineOpts opts fs = buildEnv opts fs


--------------------------------------------------------------------------------
-- Step 2 
--
-- Parse the asdl files and post-process them 
--------------------------------------------------------------------------------

processAsdlFiles :: [FilePath] -> CompilerM (ResultF AsdlSpec)
processAsdlFiles files = do
  ans  <-  readAsdlFiles files
  further postProcess ans

postProcess :: [CS.AltThree] ->  CompilerM (AsdlSpec)
postProcess xs = do
  reportEllipsis "Postprocessing input files..."
  (ts,ps,vs) <- partitionSpecs xs
  update (\s -> s { view_table = (buildViewTable vs)} )
  -- primitive types to do
  coredefs <- reduceToCore ts
  return (coredefs)
 

readAsdlFiles :: [FilePath] -> CompilerM (ResultF [CS.AltThree])
readAsdlFiles files = readAcc files []
  where
  readAcc []         acc = return (Right (concat acc))
  readAcc (file:xs)  acc = do
    reportEllipsis $ "Parsing " ++ file 
    ans <- liftIO $ parseAsdl file
    case ans of
      Left err -> return (Left err)
      Right a  -> readAcc xs (a:acc)




partitionSpecs :: (Monad m) => [CS.AltThree] -> m ([CS.ModuleDefn],[CS.PrimModule],[CS.ViewDefn])  
partitionSpecs = return . foldr part ([],[],[]) 
  where part (CS.Alt1 a) (tdefs,prims,views) = (a:tdefs, prims, views)
        part (CS.Alt2 a) (tdefs,prims,views) = (tdefs, a:prims, views)
        part (CS.Alt3 a) (tdefs,prims,views) = (tdefs, prims, a:views)

reduceToCore :: (Monad m) => [CS.ModuleDefn] -> m AsdlSpec
reduceToCore tdefs = return $ transformToCore tdefs


--------------------------------------------------------------------------------
-- Step 3
--
-- Output
--------------------------------------------------------------------------------

generateOutput :: AsdlSpec -> CompilerM ()
generateOutput spec = do
  output_dir <- query output_directory
  prefix <- query output_prefix
  reportEllipsis $ "Generating " ++ prefix 
  mkUuagOutput (output_dir ++ prefix)  spec
  mkOCamlOutput (output_dir ++ prefix)  spec
  makeHaskellOutput (output_dir ++ prefix)  spec
  
  
  
data GenT a b = GenT
  { fileName          :: String -> String
  , textComment       :: Commenter
  , fromCore          :: AsdlSpec -> a
  , toDoc             :: a -> Doc
  , optPickler        :: Maybe (GenP b)
  }


data GenP a = GenP
  { pklFileName       :: String -> String
  , toPklCode         :: AsdlSpec -> a
  , toDocPkl          :: a -> Doc
  }
  
  
uuag_gen :: GenT Ag b  
uuag_gen = GenT
  { fileName            = \s -> s ++ "AbsSynDEFS.ag"
  , textComment         = commentUuag
  , fromCore            = transformToUuag 
  , toDoc               = outputUuag
  , optPickler          = Nothing
  }

ocaml_gen :: GenT [TypeDefinition] Program
ocaml_gen = GenT
  { fileName            = \s -> s ++ "2.ml"
  , textComment         = commentOCaml
  , fromCore            = transformToOCaml
  , toDoc               = outputOCamlTypes
  , optPickler          = Just ocaml_pickler  
  }

ocaml_pickler :: GenP Program
ocaml_pickler = GenP
  { pklFileName         = \s -> s ++ "pkl.ml"
  , toPklCode           = \x -> makeOCamlAsdlPklCode x []
  , toDocPkl            = outputOCamlExprs
  }

generateTypeDesc :: (GenT a b) -> String -> AsdlSpec -> CompilerM ()
generateTypeDesc (GenT {fileName=fileName', 
                       textComment=textComment',
                       fromCore=fromCore', toDoc=toDoc'})  
                 name spec 
  = outputDocument doc textComment' opt_prolog opt_epilog path
  where doc = toDoc' $ fromCore' spec
        opt_prolog = Nothing
        opt_epilog = Nothing
        path = fileName' name 


generatePickler :: (GenT a b) -> String -> AsdlSpec -> CompilerM ()
generatePickler (GenT {optPickler=Nothing}) name decl = return ()
generatePickler (GenT {textComment=textComment',
                       optPickler = Just pklr }) name spec = do
  opt_prolog <- askView OCaml (Vw_Module "Cil") "pickler_prolog"
  opt_epilog <- askView OCaml (Vw_Module "Cil") "pickler_epilog"
  outputDocument doc textComment' opt_prolog opt_epilog path
  where doc = pp' $ codeGen' spec
        path = (pklFileName pklr) name         
        pp' = toDocPkl pklr
        codeGen' = toPklCode pklr
        
          
mkOCamlOutput :: String -> AsdlSpec -> CompilerM ()
mkOCamlOutput name spec = do 
  generateTypeDesc ocaml_gen name spec
  generatePickler ocaml_gen name spec
  
mkUuagOutput :: String -> AsdlSpec -> CompilerM ()
mkUuagOutput name spec = generateTypeDesc uuag_gen name spec

makeHaskellOutput :: String -> AsdlSpec -> CompilerM ()
makeHaskellOutput name spec = do
  reportEllipsis $ "Creating file " ++ path
  opt_prolog <- askView Haskell (Vw_Module "Cil") "pickler_prolog"
  opt_epilog <- askView Haskell (Vw_Module "Cil") "pickler_epilog" 
  liftIO $ outputToFile text commentHaskell opt_prolog opt_epilog path
  where
    -- text = error (show spec)
    text = ppHsModule $ makeHsPicklerCode spec ("Cil")
    path = name ++ "Pkl.hs"
    

outputDocument :: Doc -> Commenter -> (Maybe String) -> (Maybe String) 
                      -> FilePath -> CompilerM ()
outputDocument doc commenter opt_prolog opt_epilog path = do
  reportEllipsis $ "Creating file " ++ path
  lw <- query line_width
  liftIO $ outputToFile (text lw doc) commenter opt_prolog opt_epilog path
  where
    text line_width doc = displayS (renderPretty 0.9 line_width doc) ""
    

       
  
genTimeStamp :: Commenter -> IO String
genTimeStamp commenter = do
  t <- getClockTime
  t' <- toCalendarTime t  
  return $ commenter (calendarTimeToString t') 
    
hPutBlankLine h = hPutStrLn h ""

outputToFile text commenter opt_prolog opt_epilog path = do
  h <- openFile path WriteMode
  comment <- genTimeStamp commenter
  hPutStrLn h comment
  hPutStrLn h (fromMaybe "\n" opt_prolog) 
  hPutStr h text
  replicateM_ 2 (hPutBlankLine h)
  hPutStrLn h (fromMaybe "\n" opt_epilog) 
  hClose h
      