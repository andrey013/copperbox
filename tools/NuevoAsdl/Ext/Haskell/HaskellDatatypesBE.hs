

module Ext.Haskell.HaskellDatatypesBE where

import Base.Backend
import Util.Naming

import Gen.Ext.Haskell.HaskellAbsSyn
import Gen.Ext.Haskell.HsGenDatatypes
import Gen.Ext.Haskell.OutputHaskell

haskell_datatypes :: Backend HsModule

haskell_datatypes = Backend 
  { filename      = \p n -> p ++ (u1 n) ++ "Types.hs"
  , generate      = makeHsDatatypes
  , prettyprint   = outputHaskell
  } 

