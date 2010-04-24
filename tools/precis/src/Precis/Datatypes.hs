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
  , CabalPrecis(..)
  , SourceModule(..)
  , sourceModule 
  , ExportPrecis(..) 
  , DCDecl(..)
  , DCExportType(..)
  ) where

import Precis.Utils

import Text.PrettyPrint.Leijen                    -- package: wl-pprint

import System.FilePath


-- Don\'t use the Name type from haskell-src-exts.
-- Precis doesn\'t need its distinction between identifiers
-- and symbols.
--

type StrName = String

data CabalPrecis = CabalPrecis {
        cp_name                 :: String,
        cp_version              :: String,
        cp_cabal_file           :: FilePath,
        cp_exposed_modules      :: [SourceModule],
        cp_internal_modules     :: [SourceModule]
      }
  deriving (Eq,Show)


data SourceModule 
      = SourceModule     { src_module_name          :: StrName,
                           src_module_path_to       :: FilePath }
      | UnresolvedModule { unresolved_module_name   :: StrName }  
  deriving (Eq,Ord,Show)


-- smart constructor

sourceModule :: String -> FilePath -> SourceModule
sourceModule name path = SourceModule name (normalise path)

data ExportPrecis = ExportPrecis {
        ep_base_module          :: StrName,
        ep_exported_modules     :: [String],
        ep_dcdecls              :: [DCDecl],
        ep_simple_decls         :: [StrName]
      }
  deriving (Eq,Show)

data DCDecl = DCDecl String DCExportType
  deriving (Eq,Show)

data DCExportType = DC_Abs | DC_Restricted | DC_Full 
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
             
instance Pretty SourceModule where
  pretty (UnresolvedModule name)     = expr "unresolved module" (text name)
  pretty (SourceModule name path_to) = 
      text "module" <+> lineBraces (name_exp <$> path_exp)
    where
      name_exp = expr "name" (text name)
      path_exp = expr "path" (text path_to) 
      

instance Pretty ExportPrecis where
  pretty (ExportPrecis name mexpos dcdecls fundecls) = 
      text "module" <+> dquotes (text name) <+> lineBraces body
    where
      body  = vsep [mods, dcs, funs]
      mods  = namedBlock "module exports"          (vsep $ map dqsemi mexpos)
      dcs   = namedBlock "data class declarations" (vsep $ map dcdecl dcdecls)
      funs  = namedBlock "simple declarations"     (vsep $ map dqsemi fundecls)

dqsemi :: String -> Doc
dqsemi  = suffixSemi . dquotes . text

dcdecl :: DCDecl -> Doc
dcdecl (DCDecl name typ) = pp typ <+> (dquotes $ text name) <> semi
  where
    pp DC_Abs         = text "opaque"
    pp DC_Restricted  = text "partial"
    pp DC_Full        = text "fully exported"

