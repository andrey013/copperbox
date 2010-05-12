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
  , sourceFileModule

  , MacroExpandedSrcFile(..)
  , ModuleParseError(..)
  , moduleParseErrorMsg

  , ExportItem(..)
  , exportItemName

  , InstanceDecl(..)
  , instanceDeclName

  , DatatypeDecl(..)
  , datatypeDeclName

  , TypeSigDecl(..)
  , typeSigDeclName

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
cabalFileErrorMsg (ERR_CABAL_FILE_MISSING s) = "*** Error: missing file - " ++ s
cabalFileErrorMsg (ERR_CABAL_FILE_PARSE   s) = "*** Error: parse error - " ++ s


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



sourceFileModule :: SourceFile -> StrName
sourceFileModule (SourceFile n _)   = n
sourceFileModule (UnresolvedFile n) = n     -- defer to unresolved




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


data InstanceDecl = InstanceDecl 
       { class_name :: StrName
       , type_rep   :: TextRep
       , full_rep   :: TextRep
       }
  deriving (Eq,Show)

instanceDeclName :: InstanceDecl -> StrName
instanceDeclName = class_name


data DatatypeDecl = DatatypeDecl
      { datatype_name :: StrName
      , datatype_rep  :: TextRep
      }
  deriving (Eq,Show)

datatypeDeclName :: DatatypeDecl -> StrName
datatypeDeclName = datatype_name


data TypeSigDecl = TypeSigDecl
      { type_decl_name  :: StrName
      , type_signature  :: TextRep
      }
  deriving (Eq,Show)

typeSigDeclName :: TypeSigDecl -> StrName
typeSigDeclName = type_decl_name
