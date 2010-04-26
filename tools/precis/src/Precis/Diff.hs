{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Diff
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


module Precis.Diff
  (
    Diff(..)

  , summarizeTopLevelChanges
  , summarizeModuleDiffs

  , moduleDiffLists

  , compareModules
  , statsSourceFiles

  ) where

import Precis.Datatypes

import Text.PrettyPrint.Leijen                    -- package: wl-pprint

import Data.Map ( insert, insertWith, elems )
import qualified Data.Map as Map

data Diff a = InL a | InBoth a a | InR a
  deriving (Eq,Ord,Show)



summarizeTopLevelChanges :: CabalPrecis -> CabalPrecis -> Doc 
summarizeTopLevelChanges new_cp old_cp = 
   statsSourceFiles $ compareModules (exposed_modules new_cp) 
                                     (exposed_modules old_cp)


moduleDiffLists :: CabalPrecis 
                -> CabalPrecis 
                -> ([Diff SourceFile],[Diff SourceFile])
moduleDiffLists new_cp old_cp = (expos,privs)
  where
    expos = compareModules (exposed_modules  new_cp) (exposed_modules  old_cp)
    privs = compareModules (internal_modules new_cp) (internal_modules old_cp)


-- note - (==) on source file name, not the whole datatype
--
compareModules :: [SourceFile] -> [SourceFile] -> [Diff SourceFile]
compareModules xs ys = elems $ foldr insR `flip` ys $ foldr insL Map.empty xs
  where
    insL a s = insert (module_name a) (InL a) s
    insR b s = insertWith merge (module_name b) (InR b) s

    merge (InR a) (InL b) = InBoth a b
    merge _       _       = error "compareModules - (not) unreachable?"

statsSourceFiles :: [Diff SourceFile] -> Doc
statsSourceFiles diffs = 
        msgFileCount (length added)     <+> text "added(+),"
    <+> msgFileCount (length removed)   <+> text "removed(-)."
    <$> indent 2 (vsep (map ppAdded added ++ map ppRemoved removed))
  where
    (added,removed)    = foldr fn ([],[]) diffs
    
    fn (InL a) (xs,ys) = (a:xs,ys)
    fn (InR a) (xs,ys) = (xs,a:ys)
    fn _       acc     = acc

    ppAdded x          = char '+' <> text (module_name x)
    ppRemoved x        = char '-' <> text (module_name x)

   
msgFileCount :: Int -> Doc
msgFileCount 1 = text "1 file"
msgFileCount n = int n <+> text "files"


--------------------------------------------------------------------------------


-- Note Eq on Haskell-src-ext type will not work as Eq includes SrcLoc

summarizeModuleDiffs :: ModuleDict -> ModuleDict -> Doc
summarizeModuleDiffs new_dict old_dict = vsep $ map fn $ Map.toAscList new_dict
  where
    fn (name,Left msg)  =  text "*** ERROR -" <+> text name
                       <$> text "***"         <+> text msg

    fn (name, Right mp) = moduleDifferences name mp (findModule name old_dict)

findModule :: StrName -> ModuleDict -> (Either ModuleParseErr ModulePrecis)
findModule name dict = case Map.lookup name dict of
                         Nothing -> Left $ "missing conterpart - " ++ name
                         Just a  -> a

moduleDifferences :: StrName 
                  -> ModulePrecis 
                  -> Either ModuleParseErr ModulePrecis
                  -> Doc
moduleDifferences name _      (Left err)     = 
    text "*** ERROR -" <+> text name                                    <$> 
    text "*** Problem with corresponding module in old cabal file"      <$>
    text "***" <+> text err 
    
moduleDifferences name new_mp (Right old_mp) = 
    text "Comparing" <+> text name <$> vsep (map fn exp_items) 
  where
    exp_items = mep_exports $ mp_export_precis new_mp
    old_dmap  = mp_decls_map old_mp
    new_dmap  = mp_decls_map new_mp
    fn a      = itemDiff a new_dmap old_dmap



itemDiff :: ExportItem -> DeclMap -> DeclMap -> Doc
itemDiff item new_dmap old_dmap = step item
  where
    step (ModuleExport n)   = compareItem n new_dmap old_dmap
    step (DataOrClass  n _) = compareItem n new_dmap old_dmap
    step (Variable     n)   = compareItem n new_dmap old_dmap

compareItem :: StrName -> DeclMap -> DeclMap -> Doc
compareItem name new old = maybe fk1 sk1 $ Map.lookup name new
  where
    fk1           = text name <+> text "definition not found"
    sk1 nrep      = maybe fk2 (sk2 nrep) $ Map.lookup name old

    fk2           = text name <+> text "diff - no old definition"
    sk2 nrep orep | nrep == orep = text name <+> text "matches"
                  | otherwise    =  text name 
                                <$> text "new:" <+> text nrep
                                <$> text "old:" <+> text orep