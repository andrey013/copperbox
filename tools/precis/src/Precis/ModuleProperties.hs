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

  --
  , InstancesProp
  , instancesProp
  , diffInstancesProps

  -- 
  , DataDeclsProp
  , dataDeclsProp
  , diffDataDeclsProps

  , TypeSigsProp
  , typeSigsProp
  , diffTypeSigsProps

  ) where

import Precis.Datatypes
import Precis.HsSrcUtils
import Precis.Properties

import Language.Haskell.Exts hiding ( name, op )    -- package: haskell-src-exts

import Data.Maybe ( catMaybes )



--------------------------------------------------------------------------------
-- all modules (exposed and internal) in a cabal file

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
-- exposed modules in cabal file

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
-- export lists


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


--------------------------------------------------------------------------------
-- class instances

type InstancesProp = Property InstancesList

makeInstancesProp :: InstancesList -> InstancesProp
makeInstancesProp  = Property name descr 
  where
    name  = "Instance declarations"
    descr = "Instance declarations defined in the module."


type InstancesList = [InstanceDecl]

instancesProp :: Module -> InstancesProp
instancesProp modu = makeInstancesProp (instancesList modu)

instancesList :: Module -> InstancesList
instancesList (Module _ _ _ _ _ _ ds) = catMaybes $ map makeInstanceDecl ds

makeInstanceDecl :: Decl -> Maybe InstanceDecl
makeInstanceDecl d@(InstDecl _ _ name typs _) = 
    Just $ InstanceDecl (extractQName name) (hsppList typs) (prettyPrint d)
makeInstanceDecl _                          = Nothing


-- compare instances on class name and text rep of type
--
type InstanceKey = (StrName,TextRep)  

instanceKey :: InstanceDecl -> InstanceKey
instanceKey (InstanceDecl s k _) = (s,k)

diffInstancesProps :: InstancesProp -> InstancesProp -> [Edit InstanceDecl]
diffInstancesProps = diffProperty cmp
  where
    cmp :: InstancesList -> InstancesList -> [Edit InstanceDecl]
    cmp xs1 xs2 = difference (lift2a (==)) (/=) xs1 xs2
        
    lift2a :: (InstanceKey -> InstanceKey -> b) 
           -> InstanceDecl -> InstanceDecl -> b
    lift2a op s1 s2 = instanceKey s1 `op` instanceKey s2



--------------------------------------------------------------------------------
-- exported data types (regular and GADTS)

type DataDeclsProp = Property DataDeclsList

makeDataDeclsProp :: DataDeclsList -> DataDeclsProp
makeDataDeclsProp = Property name descr 
  where
    name  = "Exported data declarations"
    descr = "Data declarations exported from exposed modules."

type DataDeclsList = [DatatypeDecl]

dataDeclsProp :: Module -> DataDeclsProp
dataDeclsProp modu = makeDataDeclsProp (dataDeclsList modu)

dataDeclsList :: Module -> DataDeclsList
dataDeclsList (Module _ _ _ _ mb_expo _ ds) = filterDatatypes mb_expo all_datas
  where  
    all_datas = catMaybes $ map makeDatatypeDecl ds

makeDatatypeDecl :: Decl -> Maybe DatatypeDecl
makeDatatypeDecl d@(DataDecl _ _ _ name _ _ _)    = 
    Just $ DatatypeDecl (extractName name) (prettyPrint d)
makeDatatypeDecl d@(GDataDecl _ _ _ name _ _ _ _) = 
    Just $ DatatypeDecl (extractName name) (prettyPrint d)
makeDatatypeDecl _                          = Nothing

filterDatatypes :: Maybe [ExportSpec] -> [DatatypeDecl] -> [DatatypeDecl]
filterDatatypes Nothing      xs = xs
filterDatatypes (Just expos) xs = filter fn xs
  where
    fn (DatatypeDecl n _)    = n `elem` expo_vars
    expo_vars                = catMaybes $ map mkExpoT expos

    mkExpoT (EAbs n)         = Just $ extractQName n
    mkExpoT (EThingAll n)    = Just $ extractQName n
    mkExpoT (EThingWith n _) = Just $ extractQName n
    mkExpoT _                = Nothing


diffDataDeclsProps :: DataDeclsProp -> DataDeclsProp -> [Edit DatatypeDecl]
diffDataDeclsProps = diffProperty cmp
  where
    cmp :: DataDeclsList -> DataDeclsList -> [Edit DatatypeDecl]
    cmp xs1 xs2 = difference (lift2a (==)) (/=) xs1 xs2
        
    lift2a :: (StrName -> StrName -> b) -> DatatypeDecl -> DatatypeDecl -> b
    lift2a op s1 s2 = datatypeDeclName s1 `op` datatypeDeclName s2


--------------------------------------------------------------------------------
-- exported type sigs

type TypeSigsProp = Property TypeSigsList

makeTypeSigsProp :: TypeSigsList -> TypeSigsProp
makeTypeSigsProp = Property name descr 
  where
    name  = "Exported type signatures"
    descr = unwords [ "Type signatures of functions, constants... exported"
                    , "from exposed modules."]

type TypeSigsList = [TypeSigDecl]

typeSigsProp :: Module -> TypeSigsProp
typeSigsProp modu = makeTypeSigsProp (typeSigsList modu)

typeSigsList :: Module -> TypeSigsList
typeSigsList (Module _ _ _ _ mb_expo _ ds) = filterTypeSigs mb_expo all_typesigs
  where  
    all_typesigs = concat $ map makeTypeSigDecl ds

makeTypeSigDecl :: Decl -> [TypeSigDecl]
makeTypeSigDecl (TypeSig _ ns t)    = map fn ns
  where 
    fn n = TypeSigDecl (extractName n) (prettyPrint t)
makeTypeSigDecl _                    = []

filterTypeSigs :: Maybe [ExportSpec] -> [TypeSigDecl] -> [TypeSigDecl]
filterTypeSigs Nothing      xs = xs
filterTypeSigs (Just expos) xs = filter fn xs
  where
    fn (TypeSigDecl n _)     = n `elem` expo_vars
    expo_vars                = catMaybes $ map mkExpoT expos

    mkExpoT (EVar n)         = Just $ extractQName n
    mkExpoT _                = Nothing


diffTypeSigsProps :: TypeSigsProp -> TypeSigsProp -> [Edit TypeSigDecl]
diffTypeSigsProps = diffProperty cmp
  where
    cmp :: TypeSigsList -> TypeSigsList -> [Edit TypeSigDecl]
    cmp xs1 xs2 = difference (lift2a (==)) (/=) xs1 xs2
        
    lift2a :: (StrName -> StrName -> b) -> TypeSigDecl -> TypeSigDecl -> b
    lift2a op s1 s2 = typeSigDeclName s1 `op` typeSigDeclName s2
