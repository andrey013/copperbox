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
    ExportsErr
  , exposedModules
  , readModule

  ) where

import Precis.Datatypes
import Precis.HsSrcUtils
import Precis.PathUtils

import Language.Haskell.Exts hiding (name)        -- package: haskell-src-exts

import qualified Data.Map as Map




type ExportsErr = String


exposedModules :: CabalPrecis -> IO (Either ExportsErr [ModulePrecis])
exposedModules (CabalPrecis _ _ loc xs _) = step id xs
  where
    step f []     = return (Right $ f [])
    step f (n:ns) = readModule (mkPath n) (src_file_name n) >>= \ans -> 
                    case ans of 
                      Left err -> return (Left err)
                      Right a  -> step (f . (a:)) ns 

    mkPath n      = resolveToCabalFileLoc loc (src_file_path_to n)


readModule :: FilePath -> StrName -> IO (Either ExportsErr ModulePrecis)
readModule src_file modu_name = 
    bracketSourceFile src_file (pk mkPrecis mkExports)
  where
    mkPrecis  = precisExports modu_name . moduleExports

    mkExports p ast = exportedDecls (mep_exports p) (moduleDecls ast) 
    pk f g a = let x = f a in ModulePrecis  x (g x a)


bracketSourceFile :: FilePath
                  -> (Module -> a) 
                  -> IO (Either ExportsErr a)
bracketSourceFile src_file sk = do 
    ans <- parseFileWithExts knownExtensions src_file
    case ans of
      ParseFailed _ msg -> return $ Left msg
      ParseOk a         -> return $ Right $ sk a


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

