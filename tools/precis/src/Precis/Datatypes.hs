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
 
  ) where

import Text.PrettyPrint.Leijen

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
      
-- helpers
expr :: String -> Doc -> Doc
expr field body = text field <+> equals <+> (dquotes body) <> semi

lineBraces :: Doc -> Doc
lineBraces body = lbrace <> line <> indent 2 body <> line <> rbrace
 