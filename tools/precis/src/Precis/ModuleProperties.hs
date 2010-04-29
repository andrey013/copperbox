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
  , diffExportsProps

  ) where

import Precis.Datatypes
import Precis.HsSrcUtils
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

type ExportsProp = Property ExportsList

makeExportsProp :: ExportsList -> ExportsProp
makeExportsProp  = Property name descr 
  where
    name  = "Module exports"
    descr = "Explicit exports from a module (class instances not counted)."


type ExportsList = [ExportItem]


exportsProp :: Module -> ExportsProp
exportsProp modu = makeExportsProp (exportsList modu)


exportsList :: Module -> ExportsList
exportsList (Module _ _ _ _ mb_expos _ _) = 
    maybe [] (map makeExportItem)  mb_expos 


makeExportItem :: ExportSpec -> ExportItem
makeExportItem (EModuleContents name) = ModuleExport $ extractModuleName name 
makeExportItem (EVar name)            = Variable $ extractQName name
makeExportItem s@(EAbs name)          = 
    DataOrClass (extractQName name) (prettyPrint s)
makeExportItem s@(EThingAll name)     = 
    DataOrClass (extractQName name) (prettyPrint s)
makeExportItem s@(EThingWith name _)  = 
    DataOrClass (extractQName name) (prettyPrint s)


diffExportsProps :: ExportsProp -> ExportsProp -> [Edit ExportItem]
diffExportsProps = diffProperty cmp
  where
    cmp :: ExportsList -> ExportsList -> [Edit ExportItem]
    cmp es1 es2 = difference (lift2a (==)) (/=) es1 es2         
        

    lift2a :: (StrName -> StrName -> b) -> ExportItem -> ExportItem -> b
    lift2a op s1 s2 = exportItemName s1 `op` exportItemName s2
