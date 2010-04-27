{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.ModuleExports
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
--
--------------------------------------------------------------------------------


module Precis.ModuleExports 
  (
    readModule

  ) where

import Precis.Datatypes
import Precis.HsSrcUtils

import Language.Haskell.Exts hiding (name)        -- package: haskell-src-exts

import qualified Data.Map as Map


readModule :: StrName 
           -> MacroExpandedSrcFile 
           -> Either ModuleParseError ModulePrecis
readModule modu_name mx_src = 
    bracketSourceFile mx_src (pk mkPrecis mkExports)
  where
    mkPrecis  = precisExports modu_name . moduleExports

    mkExports p ast = exportedDecls (mep_exports p) (moduleDecls ast) 
    pk f g a = let x = f a in ModulePrecis  x (g x a)


bracketSourceFile :: MacroExpandedSrcFile
                  -> (Module -> a) 
                  -> Either ModuleParseError a
bracketSourceFile (MacroExpandedSrcFile filename txt) sk =
    case parseModuleWithExts knownExtensions filename txt of
      ParseFailed _ msg -> Left (ERR_MODULE_FILE_PARSE msg)
      ParseOk a         -> Right $ sk a


-- Helpers
moduleExports :: Module -> [ExportSpec]
moduleExports (Module _ _ _ _ mb_expos _ _) = maybe [] id mb_expos

moduleDecls :: Module -> [Decl]
moduleDecls (Module _srcloc _name _prags _warn _mb_expos _imps ds) = ds

precisExports :: String -> [ExportSpec] -> ModuleExportPrecis
precisExports modu_name xs = 
    ModuleExportPrecis modu_name (foldr fn [] xs)
  where
    fn a acc = makeExportItem a : acc


makeExportItem :: ExportSpec -> ExportItem
makeExportItem (EModuleContents name) = ModuleExport $ extractModuleName name 
makeExportItem (EVar name)            = Variable $ extractQName name
makeExportItem s@(EAbs name)          = 
    DataOrClass (extractQName name) (prettyPrint s)
makeExportItem s@(EThingAll name)     = 
    DataOrClass (extractQName name) (prettyPrint s)
makeExportItem s@(EThingWith name _)  = 
    DataOrClass (extractQName name) (prettyPrint s)



exportedDecls :: [ExportItem] -> [Decl] -> DeclMap
exportedDecls exps decls = foldr fn Map.empty exps
  where
    all_decls = makeDeclsMap decls
    fn (ModuleExport _)     acc = acc
    fn (Variable name)      acc = 
      maybe acc (\v -> Map.insert name v acc) $ Map.lookup name all_decls
    fn (DataOrClass name _) acc = 
      maybe acc (\v -> Map.insert name v acc) $ Map.lookup name all_decls

makeDeclsMap :: [Decl] -> DeclMap
makeDeclsMap = foldr fn Map.empty 
  where
    fn d fm = foldr (\(k,v) acc -> Map.insert k v acc) fm $ namedDecls d

