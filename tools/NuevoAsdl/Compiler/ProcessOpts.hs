{-# OPTIONS -fglasgow-exts #-}

module Compiler.ProcessOpts
  ( Flag (..)
  , header
  , options
  , processCmdLineOpts
  ) where

-- import Compiler.Datatypes
import Compiler.Env
import Compiler.Util
import Compiler.Language
import Util.ParseExt

import qualified Control.Exception as C
import Control.Monad.Error
import Control.Monad.Writer
import Data.List
import System.Console.GetOpt
import System.Directory 


data Flag
  = Flag_usage
  | Flag_target_asdl
  | Flag_target_uuag
  | Flag_target_ocaml
  | Flag_target_all
  | Flag_out_dir          String
  | Flag_silent_mode 
  | Flag_pp_width         String
  | Flag_prefix_name      String  
  deriving (Eq, Show)
  

header = "Usage: nuevoasdl.exe [OPTION...] file"

options :: [OptDescr Flag]
options =
    [ Option ['h']  ["help"]        (NoArg Flag_usage)
      "show this help"
    
    , Option []     ["asdl"]        (NoArg Flag_target_asdl) 
      "parse and pretty print to ASDL 1.2"
    
    , Option []     ["caml"]        (NoArg Flag_target_ocaml)
      "generate OCaml picklers"       
    
    , Option []     ["uuag"]        (NoArg Flag_target_uuag)
      "generate datatypes for UUAG"
    
    , Option ['a']  ["all"]         (NoArg Flag_target_all)
      "generate all targets"
      
    , Option ['d']  ["dir"]         (ReqArg Flag_out_dir "output")
      "output directory" 
    
    , Option ['s']  ["silent"]      (NoArg Flag_silent_mode)
      "silent mode" 
      
    , Option ['w']  ["width"]       (ReqArg Flag_pp_width "80")
      "line width for pretty printing" 
      
    , Option ['n']  ["name"]        (ReqArg Flag_prefix_name "output")
      "name prefix for the type and pickle files"            
    ]
      
type BuildResult = ResultE CompilerEnv 



processCmdLineOpts :: (MonadIO m) => [Flag] -> [FilePath] -> m BuildResult
processCmdLineOpts opts files = do
  e1 <- exitIfLeft $ processFiles files default_env
  e2 <- exitIfLeft $ processOptions opts e1
  checkForDefaultTarget e2

              

-- note: missing files are causeing a real exception and are not
-- caught in the monad...
processFiles :: (MonadIO m) => [FilePath] -> CompilerEnv -> m BuildResult 
processFiles files env = do
  fs <- liftIO $ checkFiles files
  return (Right $ env {input_files=fs})




checkFiles :: [FilePath] -> IO [FilePath]
checkFiles fs = foldM chk [] fs
  where 
    chk acc a = do exists <- doesFileExist a
                   case exists of 
                      True -> return $ nub $ (a:acc)
                      False -> throwError $ userError $ "The file " ++ a ++ " does not exist"
                      

  
processOptions :: (MonadIO m) => [Flag] -> CompilerEnv -> m BuildResult 
processOptions opts env = next (Right env) opts
  where
    next :: (MonadIO m) => BuildResult -> [Flag] -> m BuildResult 
    next (Right env) (o:opts) = do { ans <- processOpt o env
                                   ; next ans opts }
    next ans         []       = return ans
    next err         _        = return err





processOpt :: (MonadIO m) => Flag -> CompilerEnv -> m BuildResult 
processOpt (Flag_target_all)      env   = return $ Right (env { targets = all_targets })

processOpt (Flag_target_asdl)     env   = addTarget Asdl_1_2 env
processOpt (Flag_target_uuag)     env   = addTarget Uuag env
processOpt (Flag_target_ocaml)    env   = addTarget OCaml env

processOpt (Flag_out_dir dir)     env   = setOutDir dir env

processOpt (Flag_pp_width s)      env   = setWidth s env


processOpt (Flag_prefix_name str) env   = return $ Right (env { output_prefix = str })

-- some flags (e.g.) Flag_silent_mode aren't build into the env
-- so ignore them
processOpt _                      env   = return $ Right env

        

addTarget :: (MonadIO m) => Lang -> CompilerEnv -> m BuildResult
addTarget t env = case (t `elem` ts) of 
    True -> return $ Right env
    False -> return $ Right (env {targets=(t:ts)})
  where ts = targets env

    
    
  
setWidth :: (Monad m) => String -> CompilerEnv -> m BuildResult 
setWidth s env = return $ Right (env { line_width = i})
  where i = readIntDefault 80 s
    

setOutDir :: (MonadIO m) => String -> CompilerEnv -> m BuildResult 
setOutDir dir env = do  
  b <- liftIO $ doesDirectoryExist dir
  case b of
    True -> return $ Right (env { output_directory = (endInSlash dir) })
    _ -> return $ Left $ "--dir - bad arg '" ++ dir ++ "' using current"
  where
    endInSlash path 
      | last path == '/'    = path
      | otherwise           = path ++ "/"



checkForDefaultTarget :: (MonadIO m) => CompilerEnv -> m BuildResult
checkForDefaultTarget env   
  | null (input_files env)  = return $ Left "No input files"
  | null (targets env)      = return $ Right (env { targets = all_targets })
  | otherwise               = return $ Right env





    
                                     