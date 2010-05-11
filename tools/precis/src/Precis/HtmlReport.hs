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
import Precis.ReportMonad
import Precis.Utils

import Text.XHtml               -- package: xhtml

import Control.Monad

-- TextSummary to be printed to the console...
type TextSummary = String

makeHtmlReport :: CabalPrecis -> CabalPrecis -> IO (Html,TextSummary)
makeHtmlReport new_cp old_cp = liftM post $ execReportM MSG_AND_HTML $ 
   do { packageNamesAndVersions new_cp old_cp
      ; moduleCountSummary      new_cp old_cp
      }
  where
    post :: Log -> (Html,TextSummary)
    post (ss,hs) = (concatHtml hs, unlines ss)


packageNameAndVersion :: CabalPrecis -> ReportM ()
packageNameAndVersion cp = 
    do { tellHtml $ p << msg ; tellMsg  msg }
  where 
    msg = package_name cp ++ " " ++ package_version cp



packageNamesAndVersions :: CabalPrecis -> CabalPrecis -> ReportM ()
packageNamesAndVersions new_cp old_cp = 
    do { tellHtml $ p << (unwords xs) }
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


moduleCountSummary :: CabalPrecis -> CabalPrecis -> ReportM ()
moduleCountSummary new_cp old_cp = tellHtml $ concatHtml
    [ p << "Exposed modules:"
    , p << summarizeAR "file" "files" id expos
    , p << "Internal modules:"
    , p << summarizeAR "file" "files" id privs
    ]
  where
    new_pm        = packageModulesProp new_cp
    old_pm        = packageModulesProp old_cp
    (expos,privs) = diffPackageModulesProps new_pm old_pm


