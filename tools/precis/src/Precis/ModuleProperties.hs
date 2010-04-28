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
  , ModulesList(..)
  , diffModulesProps

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

import Language.Haskell.Exts hiding ( name )      -- package: haskell-src-exts

type PackageModulesProp = Property ModulesList

data ModulesList = ModulesList
      { public_modules  :: [StrName]
      , private_modules :: [StrName]
      }
  deriving (Eq,Show)

makePackageModulesProp :: ModulesList -> PackageModulesProp
makePackageModulesProp  = Property name descr 
  where
    name  = "Modules list"
    descr = "Lists of exposed and internal modules in a package."

packageModulesProp :: CabalPrecis -> PackageModulesProp
packageModulesProp = makePackageModulesProp . modulesList

modulesList :: CabalPrecis -> ModulesList
modulesList cp = ModulesList expos privs
  where
    expos = map sourceFileName $ exposed_modules  cp
    privs = map sourceFileName $ internal_modules cp


sourceFileName :: SourceFile -> StrName
sourceFileName (SourceFile n _)   = n
sourceFileName (UnresolvedFile n) = n 

diffModulesProps :: PackageModulesProp 
                   -> PackageModulesProp 
                   -> ([Edit StrName],[Edit StrName])
diffModulesProps = diffProperty cmp
  where
    cmp :: ModulesList -> ModulesList -> ([Edit StrName],[Edit StrName])
    cmp (ModulesList expos privs) (ModulesList expos' privs') = (xs,ys)
       where
         xs = difference (==) (/=) expos expos'         
         ys = difference (==) (/=) privs privs'

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