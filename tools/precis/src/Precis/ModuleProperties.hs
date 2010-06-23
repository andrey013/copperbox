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
   
    diffExposedModules
  , diffInternalModules
  , diffExposedSrcFiles

  --
  , diffExports

  --
  , diffInstances

  -- 
  , diffDataDecls

  , diffTypeSigs

  ) where

import Precis.Datatypes
import Precis.Diff
import Precis.HsSrcUtils

import Language.Haskell.Exts hiding ( name, op )    -- package: haskell-src-exts

import Data.Maybe ( catMaybes )



--------------------------------------------------------------------------------
-- all modules (exposed and internal) in a cabal file


diffExposedModules :: CabalPrecis -> CabalPrecis -> [Edit3 ModName]
diffExposedModules new old = diff3 (==) new' old'
  where
    new' = map getName $ exposed_modules new
    old' = map getName $ exposed_modules old

diffInternalModules :: CabalPrecis -> CabalPrecis -> [Edit3 ModName]
diffInternalModules new old = diff3 (==) new' old'
  where
    new' = map getName $ internal_modules new
    old' = map getName $ internal_modules old

getName :: SourceFile -> ModName
getName (SourceFile m _)   = m



diffExposedSrcFiles :: [SourceFile] -> [SourceFile] -> [Edit4 SourceFile]
diffExposedSrcFiles new old = diff4 equal (/=) new old 
  where
    s `equal` t = getName s == getName t

--------------------------------------------------------------------------------
-- export lists



diffExports :: Module -> Module -> [Edit4 ExportItem]
diffExports new old = diff4 equal (/=) (exportsList new) (exportsList old)         
  where      
    equal s1 s2 = exportItemName s1 == exportItemName s2


exportsList :: Module -> [ExportItem]
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


--------------------------------------------------------------------------------
-- class instances


-- compare instances on class name and text rep of type
--
type InstanceKey = (StrName,TextRep)  

instanceKey :: InstanceDecl -> InstanceKey
instanceKey (InstanceDecl s k _) = (s,k)

diffInstances :: Module -> Module -> [Edit4 InstanceDecl]
diffInstances new old = diff4 equal (/=) (instancesList new) (instancesList old)
  where      
    equal s1 s2 = instanceKey s1 == instanceKey s2

instancesList :: Module -> [InstanceDecl]
instancesList (Module _ _ _ _ _ _ ds) = catMaybes $ map makeInstanceDecl ds

makeInstanceDecl :: Decl -> Maybe InstanceDecl
makeInstanceDecl d@(InstDecl _ _ name typs _) = 
    Just $ InstanceDecl (extractQName name) (hsppList typs) (prettyPrint d)
makeInstanceDecl _                          = Nothing



--------------------------------------------------------------------------------
-- exported data types (regular and GADTS)


diffDataDecls :: Module -> Module -> [Edit4 DatatypeDecl]
diffDataDecls new old = diff4 equal (/=) (dataDeclsList new) (dataDeclsList old)
  where      
    equal s1 s2 = datatypeDeclName s1 == datatypeDeclName s2


dataDeclsList :: Module -> [DatatypeDecl]
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



--------------------------------------------------------------------------------
-- exported type sigs


diffTypeSigs :: Module -> Module -> [Edit4 TypeSigDecl]
diffTypeSigs new old = diff4 equal (/=) (typeSigsList new) (typeSigsList old)
  where        
    equal s1 s2 = typeSigDeclName s1 == typeSigDeclName s2


typeSigsList :: Module -> [TypeSigDecl]
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
