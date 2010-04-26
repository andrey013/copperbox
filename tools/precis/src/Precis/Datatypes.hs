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
  , CabalPrecis(..)
  , SourceFile(..)
  , sourceFile 


  , MacroExpandedSrcFile(..)
  , ModuleParseErr
  , DeclMap
  , ModulePrecis(..)
  , ModuleExportPrecis(..) 
  , ExportItem(..)

  ) where

import Precis.Utils

import Text.PrettyPrint.Leijen                    -- package: wl-pprint

import qualified Data.Map as Map

import System.FilePath


-- Don\'t use the Name type from haskell-src-exts.
-- Precis doesn\'t need its distinction between identifiers
-- and symbols.
--

type StrName = String
type TextRep = String

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

--------------------------------------------------------------------------------
-- Precis for individual source files

data MacroExpandedSrcFile = MacroExpandedSrcFile
      { source_file_name    :: String
      , expanded_source     :: String
      }




type ModuleParseErr = String

type DeclMap = Map.Map StrName TextRep


data ModulePrecis = ModulePrecis
      { mp_export_precis        :: ModuleExportPrecis
      , mp_decls_map            :: DeclMap
      }
  deriving (Eq,Show)


data ModuleExportPrecis = ModuleExportPrecis 
      { mep_base_module         :: StrName
      , mep_exports             :: [ExportItem]
      }
  deriving (Eq,Show)

data ExportItem = ModuleExport StrName
                | DataOrClass  StrName TextRep
                | Variable     StrName 
  deriving (Eq,Show)







--------------------------------------------------------------------------------
-- instances

instance Pretty CabalPrecis where
  pretty a = text "precis" <+> lineBraces body
    where
      body =  expr "name"     (text $ package_name a)
          <$> expr "version"  (text $ package_version a)  
          <$> expr "location" (text $ path_to_cabal_file a)
          <$> text "exposed modules"  <+> (modsbody $ exposed_modules  a)
          <$> text "internal modules" <+> (modsbody $ internal_modules a)
      modsbody = lineBraces . vsep . map pretty 
             
instance Pretty SourceFile where
  pretty (UnresolvedFile name)     = expr "unresolved file" (text name)
  pretty (SourceFile name path) = 
      text "module" <+> lineBraces (name_exp <$> path_exp)
    where
      name_exp = expr "name" (text name)
      path_exp = expr "path" (text path) 
      

instance Pretty ModulePrecis where
  pretty (ModulePrecis mep decl_map) = 
      pretty mep <$> comment_sep <$> vsep (map fn $ Map.toAscList decl_map)
    where
      fn (_,v)    = string v
      comment_sep = text (replicate 40 '%') 

 
instance Pretty ModuleExportPrecis where
  pretty (ModuleExportPrecis name expos) = 
      text "module" <+> dquotes (text name) <+> lineBraces body
    where
      body  = vsep (map pretty expos)

instance Pretty ExportItem where
  pretty (ModuleExport name) = text "module:" <+> text name
  pretty (DataOrClass _ rep) = text rep
  pretty (Variable name)     = text name

