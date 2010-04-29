{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  LGPL (depends on CppHs)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------

module Main where

import CPP
import Precis.CabalPackage
import Precis.Datatypes
import Precis.HsSrcUtils
import Precis.ModuleProperties
import Precis.PPShowS
import Precis.Properties

import Language.Haskell.Exts ( Module )         -- package: haskell-src-exts

import System.Environment
import System.Exit
import System.Console.GetOpt

-- | REMEMBER TO CHANGE THIS!
-- *****************************************************************************
version_number :: String
version_number = "0.3.0"

header :: String
header = "Usage: precis <new_cabal_file> <old_cabal_file>"

help_message :: String
help_message = unlines $  
    [ "Summarize the API differences between two revisions of a" 
    , "Cabal package."
    ]

data Flag = Usage
          | Version
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)        help_message
    , Option ['v'] ["version"]  (NoArg Version)      "show version"
    ]

main :: IO ()
main = do { args <- getArgs
          ; let (opts, nonopts, errs) = getOpt Permute options args
          ; main2 opts nonopts errs
          }
  
main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts _           _ 
  | Usage       `elem` opts = precisExit $ usageInfo header options
  | Version     `elem` opts = precisExit $ 
                                "precis version " ++ version_number

main2 _    [newc,oldc] []   = runCompare newc oldc
main2 _    _           errs = 
    precisExitFail 1 (concat errs ++ usageInfo header options)

precisExit :: String -> IO ()
precisExit s = putStrLn s >> exitWith ExitSuccess

precisExitFail :: Int -> String -> IO ()
precisExitFail i s = putStrLn s >> exitWith (ExitFailure i)


runCompare :: FilePath -> FilePath -> IO ()
runCompare new_cabal_file old_cabal_file = do 
    ans1 <- extractPrecis new_cabal_file known_extensions
    ans2 <- extractPrecis old_cabal_file known_extensions

    case (ans1,ans2) of
      (Right new_cp, Right old_cp) -> sk new_cp old_cp
      (Left err, _)                -> precisExitFail 2 $ cabalFileErrorMsg err
      (_, Left err)                -> precisExitFail 2 $ cabalFileErrorMsg err
  where
   sk new_cp old_cp = do 
      { printPackageNameAndVersions new_cp old_cp
      ; printModuleCountSummary new_cp old_cp
      ; compareExposedModules (exposedModulesProp new_cp) 
                              (exposedModulesProp old_cp)
      }


printPackageNameAndVersions :: CabalPrecis -> CabalPrecis -> IO ()
printPackageNameAndVersions new_cp old_cp = putShowSLine $ vsep
    [ repeatChar 50 '-'
    , packageNames    (package_name new_cp)    (package_name old_cp) 
    , repeatChar 50 '-'
    , packageVersions (package_version new_cp) (package_version old_cp)
    , newline
    ]
  where
    packageNames a b | a == b    = text a
                     | otherwise = text a <+> text "*** Warning: comparing to " 
                                          <+> text b
    packageVersions a b = text "Version:" <+> text a 
                       <+> text "compared to Version:" 
                       <+> text b

printModuleCountSummary :: CabalPrecis -> CabalPrecis -> IO ()
printModuleCountSummary new_cp old_cp = putShowSLine $ vsep
    [ text "Exposed modules:"
    , summarizeAddedRemoved "file" "files" id expos
    , text "Internal modules:"
    , summarizeAddedRemoved "file" "files" id privs
    , newline
    ]
  where
    new_pm        = packageModulesProp new_cp
    old_pm        = packageModulesProp old_cp
    (expos,privs) = diffPackageModulesProps new_pm old_pm
                                  

compareExposedModules :: ExposedModulesProp -> ExposedModulesProp -> IO ()
compareExposedModules new_expos old_expos = 
   mapM_ compareSrcFileEdit $ diffExposedModulesProps new_expos old_expos  

compareSrcFileEdit :: Edit SourceFile -> IO ()
compareSrcFileEdit (Conflict a b) = compareSourceFiles a b
compareSrcFileEdit _              = return ()


compareSourceFiles :: SourceFile -> SourceFile -> IO ()
compareSourceFiles new_sf old_sf = do 
  putShowSLine $ newline <> text (module_name new_sf) 
  new_ans <- fullParseModule new_sf
  old_ans <- fullParseModule old_sf
  case (new_ans, old_ans) of 
    (Right new_modu, Right old_modu) -> compareModules new_modu old_modu
    (Left err,_)                     -> putStrLn $ moduleParseErrorMsg err
    (_, Left err)                    -> putStrLn $ moduleParseErrorMsg err


compareModules :: Module -> Module -> IO ()
compareModules new_modu old_modu =
    compareExports   new_modu old_modu          >>
    compareInstances new_modu old_modu


compareExports :: Module -> Module -> IO ()
compareExports new_modu old_modu = putShowSLine $ vsep
    [ text "Explicit exports:"
    , summarizeAddedConflictRemoved "export" "exports" txt expos
    ]
  where
    new_expos = exportsProp new_modu
    old_expos = exportsProp old_modu
    expos     = diffExportsProps new_expos old_expos

    txt (ModuleExport s)   = s
    txt (DataOrClass  _ r) = r
    txt (Variable     s)   = s 

compareInstances :: Module -> Module -> IO ()
compareInstances new_modu old_modu = putShowSLine $ vsep
    [ text "Instances:"
    , summarizeAddedConflictRemoved "instance" "instances" txt expos
    ]
  where
    new_insts = instancesProp new_modu
    old_insts = instancesProp old_modu
    expos     = diffInstancesProps new_insts old_insts

    txt (InstanceDecl _ _ r)   = r


-- | macro-expand and parse
--
fullParseModule :: SourceFile -> IO (Either ModuleParseError Module)
fullParseModule (UnresolvedFile name) = 
    return $ Left $ ERR_MODULE_FILE_MISSING name
fullParseModule (SourceFile _ file_name) = do
    mx_src <- preprocessFile precisCpphsOptions file_name
    return $ readModule mx_src


