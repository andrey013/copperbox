{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.Datatypes
  (
    StrName
  , TextRep
  , CabalFileError(..)
  , cabalFileErrorMsg

  , CabalPrecis(..)
  , SourceFile(..)
  , sourceFile 
  , sourceFileName

  , MacroExpandedSrcFile(..)
  , ModuleParseError(..)
  , moduleParseErrorMsg

  , ExportItem(..)
  , exportItemName

  ) where


import System.FilePath


-- Don\'t use the Name type from haskell-src-exts.
-- Precis doesn\'t need its distinction between identifiers
-- and symbols.
--

type StrName = String
type TextRep = String

data CabalFileError = ERR_CABAL_FILE_MISSING FilePath
                    | ERR_CABAL_FILE_PARSE   String
  deriving (Eq,Show)


cabalFileErrorMsg :: CabalFileError -> String
cabalFileErrorMsg (ERR_CABAL_FILE_MISSING s) = 
    "*** Error: missing file - " ++ s
cabalFileErrorMsg (ERR_CABAL_FILE_PARSE   s) = 
    "*** Error: parse error - " ++ s


data CabalPrecis = CabalPrecis
      { package_name            :: StrName
      , package_version         :: String
      , path_to_cabal_file      :: FilePath
      , exposed_modules         :: [SourceFile]
      , internal_modules        :: [SourceFile]
      }
  deriving (Eq,Show)

-- 
data SourceFile
      = SourceFile     { module_name            :: StrName
                       , full_path_to           :: FilePath 
                       }
      | UnresolvedFile { unresolved_file_name   :: StrName }  
  deriving (Eq,Ord,Show)


-- smart constructor

sourceFile :: String -> FilePath -> SourceFile
sourceFile name path = SourceFile name (normalise path)



sourceFileName :: SourceFile -> StrName
sourceFileName (SourceFile n _)   = n
sourceFileName (UnresolvedFile n) = n 




--------------------------------------------------------------------------------
-- Precis for individual source files

data MacroExpandedSrcFile = MacroExpandedSrcFile
      { source_file_name    :: String
      , expanded_source     :: String
      }


-- | Module file names are derived from the cabal file.
-- So the name is tracked if it is missing...
--
data ModuleParseError = ERR_MODULE_FILE_MISSING String
                      | ERR_MODULE_FILE_PARSE   String
  deriving (Eq,Show)

moduleParseErrorMsg :: ModuleParseError -> String
moduleParseErrorMsg (ERR_MODULE_FILE_MISSING s) = 
    "*** Error: missing file - " ++ s
moduleParseErrorMsg (ERR_MODULE_FILE_PARSE   s) = 
    "*** Error: parse error - " ++ s


data ExportItem = ModuleExport StrName
                | DataOrClass  StrName TextRep
                | Variable     StrName 
  deriving (Eq,Show)

exportItemName :: ExportItem -> StrName
exportItemName (ModuleExport s)   = s
exportItemName (DataOrClass  s _) = s
exportItemName (Variable     s)   = s 
