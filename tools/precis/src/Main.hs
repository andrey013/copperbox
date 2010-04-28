{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------


module Main where

import Precis.CPP
import Precis.CabalPackage
import Precis.Datatypes
import Precis.ModuleExports
import Precis.ModuleProperties
import Precis.PPShowS
import Precis.Properties

import System.Environment
import System.Console.GetOpt




header :: String
header = "Usage: precis <new_cabal_file> <old_cabal_file>"

data Flag = Usage
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option ['h'] ["help"]     (NoArg Usage)        "no help yet"
    ]

main :: IO ()
main = do { args <- getArgs
          ; let (opts, nonopts, errs) = getOpt Permute options args
          ; main2 opts nonopts errs
          }
  
main2 :: [Flag] -> [FilePath] -> [String] -> IO ()
main2 opts [new_cabal,old_cabal] [] 
  | Usage       `elem` opts = putStrLn $ usageInfo header options
  | otherwise               = runCompare new_cabal old_cabal
  
main2  _ _ errors = putStrLn (concat errors ++ usageInfo header options)



runCompare :: FilePath -> FilePath -> IO ()
runCompare new_cabal_file old_cabal_file = do 
   new_cp <- runExtract new_cabal_file
   old_cp <- runExtract old_cabal_file

   printPackageNameAndVersions new_cp old_cp

   printModuleCountSummary new_cp old_cp

   compareExposedModules (exposedModulesProp new_cp) (exposedModulesProp old_cp)


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
    pm_new        = packageModulesProp new_cp
    pm_old        = packageModulesProp old_cp
    (expos,privs) = diffPackageModulesProps pm_new pm_old
                                  

compareExposedModules :: ExposedModulesProp -> ExposedModulesProp -> IO ()
compareExposedModules new_expos old_expos = 
   mapM_ compareExpoModule1 $ diffExposedModulesProps new_expos old_expos  

compareExpoModule1 :: Edit SourceFile -> IO ()
compareExpoModule1 (Added _)      = return ()
compareExpoModule1 (Removed _)    = return ()
compareExpoModule1 (Conflict a b) = putShowSLine $ text "conflict"
compareExpoModule1 (Same a)       = putShowSLine $ text "same"


{-

compareModuleDiff :: Diff SourceFile -> IO ()
compareModuleDiff (InL a)      = either print stat1 =<< fullParseModule a
compareModuleDiff (InR a)      = putStrLn $ (module_name a) ++ " only in old"
compareModuleDiff (InBoth n o) = do 
  ans1 <- fullParseModule n
  ans2 <- fullParseModule o
  case (ans1,ans2) of 
    (Right new,Right old) -> 
        let result = moduleDifferences (module_name n) new old
        in putDoc80 result
    (Left err, _) -> putStrLn $ show err
    (_, Left err) -> putStrLn $ show err
-}

-- | macro-expand and parse
--
fullParseModule :: SourceFile -> IO (Either ModuleParseError ModulePrecis)
fullParseModule (UnresolvedFile name) = 
    return $ Left $ ERR_MODULE_FILE_MISSING name
fullParseModule (SourceFile modu_name file_name) = do
    mx_src <- preprocessFile precisCpphsOptions file_name
    return $ readModule modu_name mx_src



stat1 :: ModulePrecis -> IO ()
stat1 (ModulePrecis ep _) = putStrLn $ (mep_base_module ep) ++ " only in new"


runExtract :: FilePath -> IO CabalPrecis
runExtract path = do
    ans <- extractPrecis path ["hs", "lhs"]
    case ans of
      Left  err -> error $ (fmt err)
      Right cfg -> return cfg
  where
    fmt ERR_CABAL_FILE_MISSING     = "*** Missing cabal file " ++ path
    fmt (ERR_CABAL_FILE_PARSE msg) = "*** Parse error: " ++ msg 

