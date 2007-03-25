-- use a seperate module for the Haskell pretty printer as it uses 
-- standard library pretty printer and not PPrint


module Ext.Haskell.PPHaskell 
  ( ppHsModule  
  )where

import Language.Haskell.Pretty
import Language.Haskell.Syntax


ppHsModule :: HsModule -> String
ppHsModule = prettyPrint
