{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.HtmlReport
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Creating a report in HTML...
--
--------------------------------------------------------------------------------


module Precis.HtmlReport
  ( 
    makeHtmlReport

  ) where

import Precis.Datatypes
import Precis.ModuleProperties
import Precis.Properties
import Precis.Utils

import Text.XHtml               -- package: xhtml

makeHtmlReport :: CabalPrecis -> CabalPrecis -> IO Html
makeHtmlReport new_cp old_cp = return $ concatHtml $ 
    [ packageNamesAndVersions new_cp old_cp
    , moduleCountSummary      new_cp old_cp
    ]


packageNameAndVersion :: CabalPrecis -> Html
packageNameAndVersion cp = p << msg
  where 
    msg = package_name cp ++ " " ++ package_version cp



packageNamesAndVersions :: CabalPrecis -> CabalPrecis -> Html
packageNamesAndVersions new_cp old_cp = p << (unwords xs)
  where
    xs = [ "Comparing", package_name new_cp, package_version new_cp
         , "to",        package_name old_cp, package_version old_cp
         ]




msgCount :: String ->  String -> String -> Int -> String
msgCount suffix single _      1  = unwords [ "1"   , single, suffix ]
msgCount suffix _      plural n  = unwords [ show n, plural, suffix ]


addedMsg :: String -> String -> Int -> String
addedMsg = msgCount "added (+)"

conflictMsg :: String -> String -> Int -> String
conflictMsg = msgCount "conflict (*)"

removedMsg :: String -> String -> Int -> String
removedMsg = msgCount "removed (-)"

summarizeADR :: String 
                              -> String 
                              -> (a -> String) -> [Edit a] -> String
summarizeADR single plural sf xs = 
    unlist [ added_msg, dif_msg, removed_msg ]
  where
    (as,cs,rs)    = addedConflictRemoved xs
    added_msg     = addedMsg    single plural (length as)
    dif_msg       = conflictMsg single plural (length cs)
    removed_msg   = removedMsg  single plural (length rs)

summarizeAR :: String 
                              -> String 
                              -> (a -> String) -> [Edit a] -> String
summarizeAR single plural sf xs = 
    unlist [ added_msg, removed_msg ]
  where
    (as,rs)        = addedRemoved xs
    added_msg     = addedMsg    single plural (length as)
    removed_msg   = removedMsg  single plural (length rs)


moduleCountSummary :: CabalPrecis -> CabalPrecis -> Html
moduleCountSummary new_cp old_cp = concatHtml
    [ p << "Exposed modules:"
    , p << summarizeAR "file" "files" id expos
    , p << "Internal modules:"
    , p << summarizeAR "file" "files" id privs
    ]
  where
    new_pm        = packageModulesProp new_cp
    old_pm        = packageModulesProp old_cp
    (expos,privs) = diffPackageModulesProps new_pm old_pm



{-
printPackageNamesAndVersions (Conflict new_name old_name) 


pvMsg :: Edit String -> Edit String -> String
pvMsg (EQU name)              (DIF new_v old_v) = 
  unwords [ "Comparing", name, new_v, "to" name, old_v ]    -- Good case

pvMsg (DIF new_name old_name) (Conflict



printPackageNameAndVersions :: CabalPrecis -> CabalPrecis -> Html
printPackageNameAndVersions new_cp old_cp = putShowSLine $ vsep
    [ repeatChar 50 '-'
    , packageNames    (package_name new_cp)    (package_name old_cp) 
    , repeatChar 50 '-'
    , packageVersions (package_version new_cp) (package_version old_cp)
    , newline
    ]
  where
    packageNames a b | a == b    = text a
                     | otherwise = text a <+> text "*** Warning: comparing to " 
                                          <+> text b
    packageVersions a b = text "Version:" <+> text a 
                       <+> text "compared to Version:" 
                       <+> text b

-}
{-
compareExports :: Module -> Module -> Html
compareExports new_modu old_modu = vsep
    [ text "Explicit exports:"
    , summarizeAddedConflictRemoved "export" "exports" txt expos
    ]
  where
    new_expos = exportsProp new_modu
    old_expos = exportsProp old_modu
    expos     = diffExportsProps new_expos old_expos

    txt (ModuleExport s)   = s
    txt (DataOrClass  _ r) = r
    txt (Variable     s)   = s 
-}