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
import Precis.HsSrcUtils
import Precis.ModuleProperties
import Precis.PPShowS
import Precis.Properties

import Language.Haskell.Exts ( Module )         -- package: haskell-src-exts


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
   mapM_ compareSrcFileEdit $ diffExposedModulesProps new_expos old_expos  

compareSrcFileEdit :: Edit SourceFile -> IO ()
compareSrcFileEdit (Conflict a b) = compareSourceFiles a b
compareSrcFileEdit _              = return ()


compareSourceFiles :: SourceFile -> SourceFile -> IO ()
compareSourceFiles new_sf old_sf = do 
  new_ans <- fullParseModule new_sf
  old_ans <- fullParseModule old_sf
  case (new_ans, old_ans) of 
    (Right new_modu, Right old_modu) -> compareModules new_modu old_modu
    (Left err,_)                     -> putStrLn $ show err
    (_, Left err)                    -> putStrLn $ show err


compareModules :: Module -> Module -> IO ()
compareModules new_modu old_modu = do 
    -- temp
    putStrLn $ show new_expos
    putStrLn $ show old_expos
  where
    new_expos = exportsProp new_modu
    old_expos = exportsProp old_modu


  

-- | macro-expand and parse
--
fullParseModule :: SourceFile -> IO (Either ModuleParseError Module)
fullParseModule (UnresolvedFile name) = 
    return $ Left $ ERR_MODULE_FILE_MISSING name
fullParseModule (SourceFile _ file_name) = do
    mx_src <- preprocessFile precisCpphsOptions file_name
    return $ readModule mx_src




runExtract :: FilePath -> IO CabalPrecis
runExtract path = do
    ans <- extractPrecis path known_extensions
    case ans of
      Left  err -> error $ (fmt err)
      Right cfg -> return cfg
  where
    fmt ERR_CABAL_FILE_MISSING     = "*** Missing cabal file " ++ path
    fmt (ERR_CABAL_FILE_PARSE msg) = "*** Parse error: " ++ msg 



