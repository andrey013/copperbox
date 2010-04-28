{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.ModuleProperties
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- 
--
--------------------------------------------------------------------------------


module Precis.ModuleProperties
  (
   
    PackageModulesProp
  , packageModulesProp
  , PackageModulesList(..)
  , diffPackageModulesProps

  , ExposedModulesProp
  , exposedModulesProp
  , ExposedModulesList
  , diffExposedModulesProps

  --
  , ExportsProp
  , exportsProp
  , ExportsPrecis(..)
  , ModuleExport
  , ClassExport(..)
  , DataExport(..)
  , VarExport

  ) where

import Precis.Datatypes
import Precis.Properties

import Language.Haskell.Exts hiding ( name, op )    -- package: haskell-src-exts

type PackageModulesProp = Property PackageModulesList

data PackageModulesList = PackageModulesList
      { public_modules  :: [StrName]
      , private_modules :: [StrName]
      }
  deriving (Eq,Show)

makePackageModulesProp :: PackageModulesList -> PackageModulesProp
makePackageModulesProp  = Property name descr 
  where
    name  = "Modules list"
    descr = "Lists of exposed and internal modules in a package."

packageModulesProp :: CabalPrecis -> PackageModulesProp
packageModulesProp = makePackageModulesProp . packageModulesList

packageModulesList :: CabalPrecis -> PackageModulesList
packageModulesList cp = PackageModulesList expos privs
  where
    expos = map sourceFileName $ exposed_modules  cp
    privs = map sourceFileName $ internal_modules cp


sourceFileName :: SourceFile -> StrName
sourceFileName (SourceFile n _)   = n
sourceFileName (UnresolvedFile n) = n 


diffPackageModulesProps :: PackageModulesProp 
                        -> PackageModulesProp 
                        -> ([Edit StrName],[Edit StrName])
diffPackageModulesProps = diffProperty cmp
  where
    cmp :: PackageModulesList -> PackageModulesList 
                              -> ([Edit StrName],[Edit StrName])
    cmp (PackageModulesList expos privs) (PackageModulesList expos' privs') = 
        (xs,ys)
       where
         xs = difference (==) (/=) expos expos'         
         ys = difference (==) (/=) privs privs'

--------------------------------------------------------------------------------

type ExposedModulesProp = Property ExposedModulesList

type ExposedModulesList = [SourceFile]

makeExposedModulesProp :: ExposedModulesList -> ExposedModulesProp
makeExposedModulesProp  = Property name descr 
  where
    name  = "Exposed modules"
    descr = "Exposed modules (with paths) in a package."

exposedModulesProp :: CabalPrecis -> ExposedModulesProp
exposedModulesProp = makeExposedModulesProp . exposed_modules


diffExposedModulesProps :: ExposedModulesProp 
                        -> ExposedModulesProp 
                        -> [Edit SourceFile]
diffExposedModulesProps = diffProperty cmp
  where
    cmp :: ExposedModulesList -> ExposedModulesList -> [Edit SourceFile]
    cmp es1 es2 = difference (lift2a (==)) (/=) es1 es2         
        

    lift2a :: (StrName -> StrName -> b) -> SourceFile -> SourceFile -> b
    lift2a op s1 s2 = sourceFileName s1 `op` sourceFileName s2

--------------------------------------------------------------------------------

type ExportsProp = Property ExportsPrecis

makeExportsProp :: ExportsPrecis -> ExportsProp
makeExportsProp  = Property name descr 
  where
    name  = "Module exports"
    descr = "Explicit exports from a module (class instances not counted)."


-- NoExports ?
data ExportsPrecis = ExportsPrecis 
       { exported_modules     :: [ModuleExport]
       , exported_classes     :: [ClassExport]
       , exported_data_defs   :: [DataExport]
       , exported_variables   :: [VarExport]
       }
  deriving (Eq,Show)

  
type ModuleExport = StrName
type VarExport    = StrName


-- Text rep of a Data export is for example @ One(..) @
-- (it is not a data definition).
--
data DataExport   = DataExport StrName TextRep
  deriving (Eq,Show)

data ClassExport  = ClassExport StrName TextRep
  deriving (Eq,Show)

exportsProp :: Module -> ExportsProp
exportsProp modu = makeExportsProp (exportsPrecis modu)


exportsPrecis :: Module -> ExportsPrecis
exportsPrecis _modu = error "exportsPrecis TODO" 