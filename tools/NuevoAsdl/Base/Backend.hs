

module Base.Backend (Backend(..)) where

import Gen.Base.AsdlCoreAbsSyn

import PPrint


data Backend a = Backend
  { filename        :: String -> String -> FilePath
  , generate        :: AsdlSpec -> a
  , prettyprint     :: a -> Doc
  }
  
  