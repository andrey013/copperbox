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
    CabalPrecis(..)
  , SourceModule(..)
  , sourceModule 
  , ExportPrecis(..) 
  , DCDecl(..)
  , DCExportType(..)
  ) where

import Precis.Utils

import Text.PrettyPrint.Leijen                    -- package: wl-pprint

import System.FilePath


data CabalPrecis = CabalPrecis {
        pkg_name                :: String,
        pkg_version             :: String,
        pkg_exposed_modules     :: [SourceModule],
        pkg_internal_modules    :: [SourceModule]
      }
  deriving (Eq,Show)


data SourceModule 
      = SourceModule     { src_module_name          :: String,
                           src_module_path_to       :: FilePath }
      | UnresolvedModule { unresolved_module_name   :: String }  
  deriving (Eq,Show)


-- smart constructor

sourceModule :: String -> FilePath -> SourceModule
sourceModule name path = SourceModule name (normalise path)

data ExportPrecis = ExportPrecis {
        expo_base_module        :: String,
        expo_exported_modules   :: [String],
        expo_dcdecls            :: [DCDecl],
        expo_simple_decls       :: [String]
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
      body =  expr "name"     (text $ pkg_name a)    
          <$> expr "version"  (text $ pkg_version a)  
          <$> text "exposed modules"  <+> (modsbody $ pkg_exposed_modules  a)
          <$> text "internal modules" <+> (modsbody $ pkg_internal_modules a)
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
      body  = mods <$> dcs <+> funs
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

