{-# OPTIONS -Wall #-}

module CabalDemo where


import Precis.CabalPackage

import Text.PrettyPrint.Leijen


demo1 :: IO ()
demo1 = do 
   ans <- extractPrecis "../samples/mtl.cabal" ["hs", "lhs"]
   case ans of
     Left err -> error $ err
     Right cfg -> putDoc $ pretty cfg

