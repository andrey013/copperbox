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
    makeShortReport
  , makeFullReport

  ) where

import Precis.Datatypes
import Precis.Diff
import Precis.StyleSheet
import Precis.ModuleProperties
import Precis.ReportMonad
import Precis.Utils

import Language.Haskell.Exts ( Module )         -- package: haskell-src-exts
import Text.XHtml hiding ( navy, maroon )       -- package: xhtml

import Control.Monad

-- TextSummary to be printed to the console...
type TextSummary = String


makeShortReport :: ModuleParseFunction 
                -> CabalPrecis 
                -> CabalPrecis 
                -> IO TextSummary
makeShortReport pf ncp ocp = liftM snd $ makeReport MSG_AND_HTML pf ncp ocp


makeFullReport :: ModuleParseFunction 
                -> CabalPrecis 
                -> CabalPrecis 
                -> IO (Html,TextSummary)
makeFullReport = makeReport MSG_AND_HTML


makeReport :: ReportLevel
           -> ModuleParseFunction 
           -> CabalPrecis 
           -> CabalPrecis 
           -> IO (Html,TextSummary)
makeReport lvl pf new old = liftM post $ execReportM pf lvl $ 
   do { packageNamesAndVersions new old
      ; moduleCountSummary      new old
      ; compareExposedModules (exposed_modules new) (exposed_modules old)
      }
  where
    post :: Log -> (Html,TextSummary)
    post (ss,hs) = (assembleDoc (package_name new) hs, unlines ss)


--------------------------------------------------------------------------------
-- 

assembleDoc :: String -> [Html] -> Html
assembleDoc pkg_name hs = docHead pkg_name +++ body << concatHtml hs

packageNamesAndVersions :: CabalPrecis -> CabalPrecis -> ReportM ()
packageNamesAndVersions new_cp old_cp = 
    do { tellHtml $ h1 << (package_name new_cp ++ " change summary") 
       ; tellHtml $ h2 << (unwords xs)
       ; tellMsg  $ unwords xs
       ; warnOnNameDiff (package_name new_cp) (package_name old_cp)
       }
  where
    xs = [ "Comparing", package_name new_cp, package_version new_cp
         , "to",        package_name old_cp, package_version old_cp
         ]


warnOnNameDiff :: String -> String -> ReportM ()
warnOnNameDiff new_name old_name 
    | new_name == old_name = return ()
    | otherwise            = do { tellHtml $ p << warn_msg; tellMsg warn_msg }
  where
    warn_msg = unwords $ [ "Warning: package names different -"
                         , new_name, "vs.", old_name ]




moduleCountSummary :: CabalPrecis -> CabalPrecis -> ReportM ()
moduleCountSummary new old = tellHtml $ concatHtml
    [ p << "Exposed modules:"
--    , p << summarizeAR "file" "files" expos
    , filesDiff expos
    , p << "Internal modules:"
--    , p << summarizeAR "file" "files" privs
    , filesDiff privs
    ]
  where
    expos = diffExposedModules  new old
    privs = diffInternalModules new old


compareExposedModules :: [SourceFile] -> [SourceFile] -> ReportM ()
compareExposedModules new old = 
   mapM_ compareSrcFileEdit $ diffExposedSrcFiles new old

compareSrcFileEdit :: Edit4 SourceFile -> ReportM ()
compareSrcFileEdit (DIF a b) = compareSourceFiles a b
compareSrcFileEdit _         = return ()



compareSourceFiles :: SourceFile -> SourceFile -> ReportM ()
compareSourceFiles new_sf old_sf = do 
    do { tellHtml $ p << module_name new_sf
       ; pf      <- askParseFun
       ; new_ans <- liftIO $ pf new_sf
       ; old_ans <- liftIO $ pf old_sf
       ; case (new_ans, old_ans) of 
           (Right new_m, Right old_m) -> compareModules new_m old_m
           (Left err,_)               -> liftIO $ putStrLn $ moduleParseErrorMsg err
           (_, Left err)              -> liftIO $ putStrLn $ moduleParseErrorMsg err
       }

compareModules :: Module -> Module -> ReportM ()
compareModules new_modu old_modu = 
    do { compareExports   new_modu old_modu
       ; compareDataDecls new_modu old_modu
       ; compareTypeSigs  new_modu old_modu
       ; compareInstances new_modu old_modu
       }


compareExports :: Module -> Module -> ReportM ()
compareExports new old = 
    do { tellHtml $ p << "Explicit exports:"
       ; tellHtml $ p << summarizeADR "export" "exports" expos
       }
  where
    expos     = diffExports new old

--    txt (ModuleExport s)   = s
--    txt (DataOrClass  _ r) = r
--    txt (Variable     s)   = s 

compareInstances :: Module -> Module -> ReportM ()
compareInstances new old = 
    do { tellHtml $ p << "Instances:"
       ; tellHtml $ p << summarizeADR "instance" "instances" expos
       }
  where
    expos     = diffInstances new old

--    txt (InstanceDecl _ _ r)   = r


compareDataDecls :: Module -> Module -> ReportM ()
compareDataDecls new old = 
    do { tellHtml $ p << "Exported data type decls:"
       ; tellHtml $ p << summarizeADR "data type" "data types" ddecls
       }
  where
    ddecls     = diffDataDecls new old


compareTypeSigs :: Module -> Module -> ReportM ()
compareTypeSigs new old = 
    do { tellHtml $ p << "Exported type sigs:"
       ; tellHtml $ p << summarizeADR "type signature" 
                                      "type signatures" tysigs
       }
  where
    tysigs     = diffTypeSigs new old

--    ppty (TypeSigDecl n r) = n ++ " :: " ++ r



--------------------------------------------------------------------------------
-- Helpers

msgCount :: String ->  String -> String -> Int -> String
msgCount suffix single _      1  = unwords [ "1"   , single, suffix ]
msgCount suffix _      plural n  = unwords [ show n, plural, suffix ]


addedMsg :: String -> String -> Int -> String
addedMsg = msgCount "added (+)"

conflictMsg :: String -> String -> Int -> String
conflictMsg = msgCount "conflict (*)"

removedMsg :: String -> String -> Int -> String
removedMsg = msgCount "removed (-)"

summarizeADR :: String -> String -> [Edit4 a] -> String
summarizeADR single plural xs = 
    unlist [ added_msg, dif_msg, removed_msg ]
  where
    (as,cs,rs)    = addedConflictRemoved xs
    added_msg     = addedMsg    single plural (length as)
    dif_msg       = conflictMsg single plural (length cs)
    removed_msg   = removedMsg  single plural (length rs)

summarizeAR :: String -> String  -> [Edit4 a] -> String
summarizeAR single plural xs = 
    unlist [ added_msg, removed_msg ]
  where
    (as,rs)       = addedRemoved xs
    added_msg     = addedMsg    single plural (length as)
    removed_msg   = removedMsg  single plural (length rs)


--------------------------------------------------------------------------------
-- Html ...

docHead :: String -> Html
docHead pkg_name = header << doc_title +++ doc_style

  where
    doc_title = thetitle << (pkg_name ++ " change summary")
    doc_style = style ! [thetype "text/css"] << inline_stylesheet


filesDiff :: [Edit3 String] -> Html
filesDiff xs = table << zipWith fn xs [1::Int ..]
  where
    fn (Add a)   i = makeRow i "+" a
    fn (Equ a)   i = makeRow i ""  a
    fn (Del a)   i = makeRow i "-" a
    
    makeRow i op name = tr << [ td << (show i), td << op, td << name ]


