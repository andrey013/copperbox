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

  , DeclMap
  , ModulePrecis(..)
  , ModuleExportPrecis(..) 
  , DcDecl(..)
  , DcExportType(..)
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
      { cp_name                 :: StrName
      , cp_version              :: String
      , cp_cabal_file           :: FilePath
      , cp_exposed_modules      :: [SourceFile]
      , cp_internal_modules     :: [SourceFile]
      }
  deriving (Eq,Show)


data SourceFile
      = SourceFile     { src_file_name          :: StrName,
                         src_file_path_to       :: FilePath }
      | UnresolvedFile { unresolved_file_name   :: StrName }  
  deriving (Eq,Ord,Show)


-- smart constructor

sourceFile :: String -> FilePath -> SourceFile
sourceFile name path = SourceFile name (normalise path)

--------------------------------------------------------------------------------
-- Precis for individual source files

type DeclMap = Map.Map StrName TextRep


data ModulePrecis = ModulePrecis
      { mp_export_precis        :: ModuleExportPrecis
      , mp_decls_map            :: DeclMap
      }
  deriving (Eq,Show)


data ModuleExportPrecis = ModuleExportPrecis 
      { mep_base_module         :: StrName
      , mep_exported_modules    :: [String]
      , mep_dcdecls             :: [DcDecl]
      , mep_simple_decls        :: [StrName]
      }
  deriving (Eq,Show)

data DcDecl = DcDecl StrName DcExportType
  deriving (Eq,Show)

data DcExportType = DC_Abs | DC_Restricted | DC_Full 
  deriving (Eq,Show)





--------------------------------------------------------------------------------
-- instances

instance Pretty CabalPrecis where
  pretty a = text "precis" <+> lineBraces body
    where
      body =  expr "name"     (text $ cp_name a)    
          <$> expr "version"  (text $ cp_version a)  
          <$> expr "location" (text $ cp_cabal_file a)
          <$> text "exposed modules"  <+> (modsbody $ cp_exposed_modules  a)
          <$> text "internal modules" <+> (modsbody $ cp_internal_modules a)
      modsbody = lineBraces . vsep . map pretty 
             
instance Pretty SourceFile where
  pretty (UnresolvedFile name)     = expr "unresolved file" (text name)
  pretty (SourceFile name path_to) = 
      text "module" <+> lineBraces (name_exp <$> path_exp)
    where
      name_exp = expr "name" (text name)
      path_exp = expr "path" (text path_to) 
      

instance Pretty ModulePrecis where
  pretty (ModulePrecis mep decl_map) = 
      pretty mep <$> comment_sep <$> vsep (map fn $ Map.toAscList decl_map)
    where
      fn (_,v)    = string v
      comment_sep = text (replicate 40 '%') 

 
instance Pretty ModuleExportPrecis where
  pretty (ModuleExportPrecis name mexpos dcdecls fundecls) = 
      text "module" <+> dquotes (text name) <+> lineBraces body
    where
      body  = vsep [mods, dcs, funs]
      mods  = namedBlock "module exports"          (vsep $ map dqsemi mexpos)
      dcs   = namedBlock "data class declarations" (vsep $ map dcdecl dcdecls)
      funs  = namedBlock "simple declarations"     (vsep $ map dqsemi fundecls)

dqsemi :: String -> Doc
dqsemi  = suffixSemi . dquotes . text

dcdecl :: DcDecl -> Doc
dcdecl (DcDecl name typ) = pp typ <+> (dquotes $ text name) <> semi
  where
    pp DC_Abs         = text "opaque"
    pp DC_Restricted  = text "partial"
    pp DC_Full        = text "fully exported"

