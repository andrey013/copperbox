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

    mkExports p ast = exportedDecls (mep_dcdecls p)
                                    (mep_simple_decls p) 
                                    (moduleDecls ast)

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
    ModuleExportPrecis modu_name expo_mods dcdecls simples
  where
    (expo_mods, dcdecls, simples) = foldr fn ([],[],[]) xs

    fn (EModuleContents name) (mods,dcs,simps) = 
        let m1 = extractModuleName name in (m1:mods, dcs, simps)

    fn (EVar name)            (mods,dcs,simps) = 
        let s1 = extractQName name in (mods, dcs, s1:simps)

    fn (EAbs name)            (mods,dcs,simps) = 
        let x1 = DcDecl (extractQName name) DC_Abs in (mods,x1:dcs,simps)

    fn (EThingAll name)       (mods,dcs,simps) = 
        let x1 = DcDecl (extractQName name) DC_Full in (mods,x1:dcs,simps)

    fn (EThingWith name _)    (mods,dcs,simps) = 
        let x1 = DcDecl (extractQName name) DC_Restricted in (mods,x1:dcs,simps)


exportedDecls :: [DcDecl] -> [StrName] -> [Decl] -> DeclMap
exportedDecls dcls names decls = foldr fn Map.empty (names ++ map extr dcls)
  where
    all_decls = makeDeclsMap decls
    extr (DcDecl n _) = n
    fn a acc  = maybe acc (\v -> Map.insert a v acc) $ Map.lookup a all_decls


makeDeclsMap :: [Decl] -> DeclMap
makeDeclsMap = foldr fn Map.empty 
  where
    fn d fm = foldr (\(k,v) acc -> Map.insert k v acc) fm $ namedDecls d

