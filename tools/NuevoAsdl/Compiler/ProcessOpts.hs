
module Compiler.ProcessOpts
  ( Flag (..)
  , header
  , options
  , buildEnv
  ) where

-- import Compiler.Datatypes
import Compiler.Env
import Compiler.CompilerMonad
import Compiler.Language
import Util.ParseExt

import qualified Control.Exception as C
import Control.Monad.Error
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
      



-- buildEnv :: [Flag] -> [FilePath] -> IO (ResultFL CompilerEnv)
buildEnv opts files = do
  reportSep
  report "Validating the command line arguments"
  processFiles files
  processOptions opts
  checkForDefaultTarget
              

-- note: missing files are causeing a real exception and are not
-- caught in the monad...
processFiles :: [FilePath] -> CompilerM () 
processFiles files = do
  fs <- lift $ do{ lift $ checkFiles files}
  update (\s -> s {input_files=fs})




checkFiles :: [FilePath] -> IO [FilePath]
checkFiles fs = foldM chk [] fs
  where 
    chk acc a = do exists <- doesFileExist a
                   case exists of 
                      True -> return $ nub $ (a:acc)
                      False -> throwError $ userError $ "The file " ++ a ++ " does not exist"
                      

  
processOptions :: [Flag] -> CompilerM ()
processOptions opts = mapM_ processOpt opts




processOpt :: Flag -> CompilerM ()
processOpt (Flag_target_all)      = update (\s -> s{ targets = all_targets })

processOpt (Flag_target_asdl)     = addTarget Asdl_1_2
processOpt (Flag_target_uuag)     = addTarget Uuag
processOpt (Flag_target_ocaml)    = addTarget OCaml


processOpt (Flag_out_dir dir)     = setOutDir dir
processOpt (Flag_pp_width s)      = setWidth s

processOpt (Flag_prefix_name str) = update (\s -> s { output_prefix = str })

-- some flags (e.g.) Flag_silent_mode aren't build into the env
-- so ignore them
processOpt _                      = return ()

        

  
setWidth :: String -> CompilerM ()
setWidth s = update (\s -> s { line_width = i})
  where i = readIntDefault 80 s
    

setOutDir :: String -> CompilerM ()
setOutDir dir = do  
  b <- liftIO $ doesDirectoryExist dir
  case b of
    True -> update (\s -> s { output_directory = (endInSlash dir) })
    _ -> throwError $ "--dir - bad arg '" ++ dir ++ "' using current"
  where
    endInSlash path 
      | last path == '/'    = path
      | otherwise           = path ++ "/"



checkForDefaultTarget :: CompilerM ()
checkForDefaultTarget = do
  xs <- query input_files
  ys <- query targets
  case (not $ null xs) && (null ys) of
    True -> update (\s -> s  { targets = all_targets })



    
                                     