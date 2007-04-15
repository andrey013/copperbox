
module Compiler.Compile
  ( runCompile
  ) where

import Compiler.Env
import Compiler.Language
import Compiler.Util
import Compiler.ParseAsdl
import Compiler.ProcessOpts
import Compiler.Views.ViewBase
import Compiler.WriteOutput
import qualified Base.AsdlConcreteSyn as CS
import Base.Lib
import Base.Backend

-- (2DO) Do we want to explicitly import from the Gen directory?
import Gen.Compiler.Asdl2Core

import Ext.UUAG.Lib
import Ext.OCaml.Lib
import Ext.Haskell.Lib
import Ext.Haskell.HaskellDatatypesBE

-- import qualified Gen.MiniHaskellAbsSyn as MiniH
-- import qualified Gen.OutputHaskell as MiniPP

import qualified LambdaCore.Lib as LCore

import Util.Naming

import PPrint

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import System.Exit
import System.IO
-- import System.Time




runCompile :: [Flag] -> [FilePath] -> IO ()
runCompile opts files = do
  env <- exitIfLeft $ processCmdLineOpts opts files
  (spec,views) <- processAsdlFiles files
  generateOutput spec env
  gen2 spec haskell_datatypes env
  genCore spec
  return ()
                 
                   
--------------------------------------------------------------------------------
-- Step 1 
--
-- Process the command line options and build the __ environment.
-- All the work done in the ProcessOpts module
--------------------------------------------------------------------------------                     
-- processCmdLineOpts :: (MonadIO m) => [Flag] -> [FilePath] -> CompilerM ()
-- processCmdLineOpts opts fs = buildEnv opts fs


--------------------------------------------------------------------------------
-- Step 2 
--
-- Parse the asdl files and post-process them 
--------------------------------------------------------------------------------

processAsdlFiles :: (MonadIO m) => [FilePath] -> m (AsdlSpec,[CS.ViewDefn])
processAsdlFiles files = do
  ans <- readAsdlFiles files
  (ts,ps,vs) <- partitionSpecs ans
  -- update (\s -> s { view_table = (buildViewTable vs)} )
  -- primitive types to do
  coredefs <- reduceToCore ts
  return (coredefs,vs)


readAsdlFiles :: (MonadIO m) => [FilePath] -> m [CS.AltThree]
readAsdlFiles files = do xss <- sequenceM_failure $ map parseAsdl files
                         return (concat xss)



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

gen2 :: (MonadIO m) => AsdlSpec -> Backend a -> CompilerEnv -> m ()
gen2 spec be env = do
  let output_dir = output_directory env
  let prefix = output_prefix env
  let outfile = (filename be) output_dir prefix
  let ast = (generate be) spec
  let doc = (prettyprint be) ast
  let text = displayS (renderPretty 0.9 80 doc) ""
  liftIO $ putStrLn $ "generating..." ++ outfile
  liftIO $ putStrLn $ text
  
genCore :: (MonadIO m) => AsdlSpec -> m ()
genCore spec = do
  let lc = LCore.makePickler spec
  let doc = LCore.outputLambdaCore lc
  let text = displayS (renderPretty 0.9 80 doc) ""
  liftIO $ putStrLn $ text
  --
  -- let lc' = fromJust $ splitCase lc
  -- splitCase maybe best as Maybe!
  let lc' = splitCase lc
  let doc' = LCore.outputLambdaCore lc'
  let text' = displayS (renderPretty 0.9 80 doc') ""
  liftIO $ putStrLn $ text'
{-  
  let hs = translateToHaskell lc "Dummy"
  let doc' = outputHaskell hs
  let text' = displayS (renderPretty 0.9 80 doc') ""
  report $ text'
-}


generateOutput :: (MonadIO m) => AsdlSpec -> CompilerEnv -> m ()
generateOutput spec env = do
  mkUuagOutput (output_dir ++ prefix) spec env
  mkOCamlOutput (output_dir ++ prefix) spec env
--  makeHaskellOutput (output_dir ++ prefix) spec env
--  makeHaskellDatatypes (output_dir ++ prefix) spec env
  where prefix      = output_prefix env
        output_dir  = output_directory env

 


  
    
  
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
  { fileName            = \s -> (u1 s) ++ "AbsSynDEFS.ag"
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

generateTypeDesc :: (MonadIO m) => (GenT a b) -> String -> AsdlSpec -> CompilerEnv -> m ()
generateTypeDesc (GenT {fileName=fileName', 
                       textComment=textComment',
                       fromCore=fromCore', toDoc=toDoc'})  
                 name spec env  
  = outputDocument doc textComment' opt_prolog opt_epilog path env 
  where doc = toDoc' $ fromCore' spec
        opt_prolog = Nothing
        opt_epilog = Nothing
        path = fileName' name 


generatePickler :: (MonadIO m) => (GenT a b) -> String -> AsdlSpec -> CompilerEnv 
                      -> m ()
generatePickler (GenT {optPickler=Nothing}) name decl env = return ()
generatePickler (GenT {textComment=textComment',
                       optPickler = Just pklr }) name spec env = do
  opt_prolog <- askView OCaml (Vw_Module "Cil") "pickler_prolog" env
  opt_epilog <- askView OCaml (Vw_Module "Cil") "pickler_epilog" env
  outputDocument doc textComment' opt_prolog opt_epilog path env
  where doc = pp' $ codeGen' spec
        path = (pklFileName pklr) name         
        pp' = toDocPkl pklr
        codeGen' = toPklCode pklr
        

askView l k1 k2 env = return $ textEntry l k1 k2 (view_table env)
  
  
          
mkOCamlOutput :: (MonadIO m) => String -> AsdlSpec -> CompilerEnv -> m ()
mkOCamlOutput name spec env = do 
  generateTypeDesc ocaml_gen name spec env
  generatePickler ocaml_gen name spec env 
  
mkUuagOutput :: (MonadIO m) => String -> AsdlSpec -> CompilerEnv -> m ()
mkUuagOutput name spec env = generateTypeDesc uuag_gen name spec env

{-
makeHaskellOutput :: (MonadIO m) => String -> AsdlSpec -> CompilerEnv -> m ()
makeHaskellOutput name spec env = do
  opt_prolog <- askView Haskell (Vw_Module "Cil") "pickler_prolog" env 
  opt_epilog <- askView Haskell (Vw_Module "Cil") "pickler_epilog" env  
  liftIO $ outputToFile text commentHaskell opt_prolog opt_epilog path
  where
    -- text = error (show spec)
    text = ppHsModule $ makeHsPicklerCode spec ("Cil")
    path = u1 name ++ "Pkl.hs"
    

makeHaskellDatatypes :: (MonadIO m) => String -> AsdlSpec -> CompilerEnv -> m ()
makeHaskellDatatypes name spec env = do
  outputDocument doc commentHaskell Nothing Nothing path
  where
    -- text = error (show spec)
    doc = MiniPP.outputMiniHaskell $ makeHsDatatypes spec
    path = u1 name ++ "Syntax.hs"
-}    

outputDocument :: (MonadIO m) => Doc -> Commenter -> (Maybe String) -> (Maybe String) 
                      -> FilePath -> CompilerEnv -> m ()
outputDocument doc commenter opt_prolog opt_epilog path env = do
  liftIO $ outputToFile (text lw doc) commenter opt_prolog opt_epilog path
  where
    text line_width doc = displayS (renderPretty 0.9 line_width doc) ""
    lw = line_width env

     
  

      