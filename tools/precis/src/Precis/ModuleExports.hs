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

type ExportsErr = String

extractExports :: SourceModule -> IO (Either ExportsErr ExportPrecis)
extractExports modu = do
    ans <- parseFileWithExts knownExtensions (src_module_path_to modu)
    case ans of
      ParseFailed _ msg -> return $ Left msg
      ParseOk a         -> return $ Right $ sk a
  where
    sk = precisExports (src_module_name modu) . maybe [] id . mbExports



-- Helpers
mbExports :: Module -> Maybe [ExportSpec]
mbExports (Module _srcloc _name _prags _warn mb_expos _imps _decls) = mb_expos


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

