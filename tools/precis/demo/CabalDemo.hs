{-# LANGUAGE ParallelListComp           #-}

module CabalDemo where

import Precis.CabalPackage
import Precis.CabalFinalize
import Precis.PathUtils
import Precis.Utils

import Distribution.Version

import Data.Monoid
import System.FilePath


v6_12 :: Version
v6_12 = Version [6,12] []

demo1 = readPackageDescr "../samples/mtl.cabal" >>= print
demo2 = readGenPackageDescr "../samples/mtl.cabal" >>= print

demo3 = getCabalPrecis "../samples/mtl.cabal" >>= print
demo4 = readGenPackageDescr "../samples/mtl.cabal" >>= print . cabalFinalize v6_12

demo5 = getCabalPrecis "../samples/mtl.cabal" >>= \cab ->
        mapM (\path -> moduleFind (fn path)  "hs") (pathPlan cab)
  where
    fn x = joinPath $ ["..","samples"] ++ splitPath x


pathPlan :: CabalPrecis -> [FilePath]
pathPlan = concatMap libPlan . pkg_libraries where


libPlan :: LibraryPrecis -> [FilePath]
libPlan lib = map (uncurry moduleLongPath) $ 
   crossPlus (lib_hs_source_dirs lib) (lib_exposed_modules lib) 


crossPlus :: Monoid a => [a] -> [b] -> [(a,b)]
crossPlus [] ys = map (\b -> (mempty,b)) ys
crossPlus xs ys = [(a,b) | a <- xs , b <- ys ]



