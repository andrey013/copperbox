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
import Precis.ModuleProperties
import Precis.Properties
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
makeReport lvl pf new_cp old_cp = liftM post $ execReportM pf lvl $ 
   do { packageNamesAndVersions new_cp old_cp
      ; moduleCountSummary      new_cp old_cp
      ; compareExposedModules (exposedModulesProp new_cp) 
                              (exposedModulesProp old_cp)
      }
  where
    post :: Log -> (Html,TextSummary)
    post (ss,hs) = (assembleDoc (package_name new_cp) hs, unlines ss)


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
moduleCountSummary new_cp old_cp = tellHtml $ concatHtml
    [ p << "Exposed modules:"
    , p << summarizeAR "file" "files" expos
    , filesDiff expos
    , p << "Internal modules:"
    , p << summarizeAR "file" "files" privs
    , filesDiff privs
    ]
  where
    new_pm        = packageModulesProp new_cp
    old_pm        = packageModulesProp old_cp
    (expos,privs) = diffPackageModulesProps new_pm old_pm



compareExposedModules :: ExposedModulesProp -> ExposedModulesProp -> ReportM ()
compareExposedModules new_expos old_expos = 
   mapM_ compareSrcFileEdit $ diffExposedModulesProps new_expos old_expos  

compareSrcFileEdit :: Edit SourceFile -> ReportM ()
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
compareExports new_modu old_modu = 
    do { tellHtml $ p << "Explicit exports:"
       ; tellHtml $ p << summarizeADR "export" "exports" expos
       }
  where
    new_expos = exportsProp new_modu
    old_expos = exportsProp old_modu
    expos     = diffExportsProps new_expos old_expos

    txt (ModuleExport s)   = s
    txt (DataOrClass  _ r) = r
    txt (Variable     s)   = s 

compareInstances :: Module -> Module -> ReportM ()
compareInstances new_modu old_modu = 
    do { tellHtml $ p << "Instances:"
       ; tellHtml $ p << summarizeADR "instance" "instances" expos
       }
  where
    new_insts = instancesProp new_modu
    old_insts = instancesProp old_modu
    expos     = diffInstancesProps new_insts old_insts

    txt (InstanceDecl _ _ r)   = r


compareDataDecls :: Module -> Module -> ReportM ()
compareDataDecls new_modu old_modu = 
    do { tellHtml $ p << "Exported data type decls:"
       ; tellHtml $ p << summarizeADR "data type" "data types" ddecls
       }
  where
    new_ddecls = dataDeclsProp new_modu
    old_ddecls = dataDeclsProp old_modu
    ddecls     = diffDataDeclsProps new_ddecls old_ddecls


compareTypeSigs :: Module -> Module -> ReportM ()
compareTypeSigs new_modu old_modu = 
    do { tellHtml $ p << "Exported type sigs:"
       ; tellHtml $ p << summarizeADR "type signature" 
                                      "type signatures" tysigs
       }
  where
    new_tysigs = typeSigsProp new_modu
    old_tysigs = typeSigsProp old_modu
    tysigs     = diffTypeSigsProps new_tysigs old_tysigs

    ppty (TypeSigDecl n r) = n ++ " :: " ++ r



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

summarizeADR :: String -> String -> [Edit a] -> String
summarizeADR single plural xs = 
    unlist [ added_msg, dif_msg, removed_msg ]
  where
    (as,cs,rs)    = addedConflictRemoved xs
    added_msg     = addedMsg    single plural (length as)
    dif_msg       = conflictMsg single plural (length cs)
    removed_msg   = removedMsg  single plural (length rs)

summarizeAR :: String -> String  -> [Edit a] -> String
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


filesDiff :: [Edit String] -> Html
filesDiff xs = table << zipWith fn xs [1::Int ..]
  where
    fn (ADD a)   i = makeRow i "+" a
    fn (DIF a b) i = makeRow i "*" (unwords [a,b])
    fn (EQU a)   i = makeRow i ""  a
    fn (DEL a)   i = makeRow i "-" a
    
    makeRow i op name = tr << [ td << (show i), td << op, td << name ]



inline_stylesheet :: Html
inline_stylesheet = primHtml $ unlines $ 
  [ "<!--"
  , "body { background-color: white; color: black; "
  , "        font-family: sans-serif; padding: 0 0; }"
  , ""
  , "h1    { background-color: " ++ whitesmoke ++ "; color: " ++ brown ++ " }"
  , "-->"
  ]
  
-- some named colours...

aliceblue :: String
aliceblue = "rgb(240,249,255)"


brown :: String 
brown = "rgb(165,43,43)"

chocolate :: String
chocolate = "rgb(211,106,31)"

crimson :: String
crimson = "rgb(221,20,60)"

maroon :: String
maroon = "rgb(128,0,0)"

mintcream :: String
mintcream = "rgb(246,255,250)"

navy :: String 
navy = "rgb(0,0,128)"

whitesmoke :: String
whitesmoke = "rgb(246,246,246)"