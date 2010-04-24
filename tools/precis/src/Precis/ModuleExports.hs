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
  , extractExports

  ) where

import Precis.Datatypes
import Precis.HsSrcUtils

import Language.Haskell.Exts hiding (name)        -- package: haskell-src-exts

import qualified Data.Map as Map

type DeclMap = Map.Map StrName Decl



type ExportsErr = String

extractExports :: SourceModule -> IO (Either ExportsErr (ExportPrecis,DeclMap))
extractExports src_mod = bracketSourceModule src_mod  (pk mkPrecis mkExports)
  where
    mkPrecis  = precisExports (src_module_name src_mod) . maybe [] id . mbExports

    mkExports p ast = exportedDecls (ep_simple_decls p) (decls ast)

    pk f g a = let x = f a in (x, g x a)


bracketSourceModule :: SourceModule 
                    -> (Module -> a) 
                    -> IO (Either ExportsErr a)
bracketSourceModule modu sk = do 
    ans <- parseFileWithExts knownExtensions (src_module_path_to modu)
    case ans of
      ParseFailed _ msg -> return $ Left msg
      ParseOk a         -> return $ Right $ sk a


-- Helpers
mbExports :: Module -> Maybe [ExportSpec]
mbExports (Module _srcloc _name _prags _warn mb_expos _imps _decls) = mb_expos

decls :: Module -> [Decl]
decls (Module _srcloc _name _prags _warn _mb_expos _imps decls) = decls

precisExports :: String -> [ExportSpec] -> ExportPrecis
precisExports modu_name xs = ExportPrecis modu_name expo_mods dcdecls simples
  where
    (expo_mods, dcdecls, simples) = foldr fn ([],[],[]) xs

    fn (EModuleContents name) (mods,dcs,simps) = 
        let m1 = extractModuleName name in (m1:mods, dcs, simps)

    fn (EVar name)            (mods,dcs,simps) = 
        let s1 = extractQName name in (mods, dcs, s1:simps)

    fn (EAbs name)            (mods,dcs,simps) = 
        let x1 = DCDecl (extractQName name) DC_Abs in (mods,x1:dcs,simps)

    fn (EThingAll name)       (mods,dcs,simps) = 
        let x1 = DCDecl (extractQName name) DC_Full in (mods,x1:dcs,simps)

    fn (EThingWith name _)    (mods,dcs,simps) = 
        let x1 = DCDecl (extractQName name) DC_Restricted in (mods,x1:dcs,simps)


exportedDecls :: [StrName] -> [Decl] -> DeclMap
exportedDecls names decls = foldr fn Map.empty names
  where
    all_decls = makeDeclsMap decls
    fn a acc  = maybe acc (\v -> Map.insert a v acc) $ Map.lookup a all_decls


makeDeclsMap :: [Decl] -> Map.Map StrName Decl
makeDeclsMap = foldr fn Map.empty 
  where
    fn d fm = foldr (\(k,v) acc -> Map.insert k v acc) fm $ namedDecls d

