-- helper functions to create HaskellAbsSyn

module Ext.Haskell.HsSynHelper 
  where

import Gen.Ext.Haskell.HaskellAbsSyn

hsImport :: String -> HsImportDecl
hsImport = HsImportDecl