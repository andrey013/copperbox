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
import Precis.PPShowS ( toString, line )
import Precis.ReportMonad
import Precis.TextOutput

import Language.Haskell.Exts ( Module )  -- package: haskell-src-exts
import Text.XHtml hiding ( name )                     -- package: xhtml

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
    post (hs,stats) = (assembleDoc (package_name new) hs, mkText stats)

    mkText stats = toString $ 
                     (comparingMsg new old) `line` showChangeStats stats
    


assembleDoc :: String -> [Html] -> Html
assembleDoc pkg_name hs = docHead pkg_name +++ body << concatHtml hs


--------------------------------------------------------------------------------
-- 

packageNamesAndVersions :: CabalPrecis -> CabalPrecis -> ReportM ()
packageNamesAndVersions new old = 
    do { tellHtml $ h1 << ("Change summary: " ++ package_name new) 
       ; tellHtml $ h2 << (toString $ comparingMsg new old)
       ; warnOnNameDiff (package_name new) (package_name old)
       }


warnOnNameDiff :: String -> String -> ReportM ()
warnOnNameDiff new_name old_name 
    | new_name == old_name = return ()
    | otherwise            = do { tellHtml $ p << warn_msg }
  where
    warn_msg = unwords $ [ "Warning: package names different -"
                         , new_name, "vs.", old_name ]




moduleCountSummary :: CabalPrecis -> CabalPrecis -> ReportM ()
moduleCountSummary new old = 
    do { countDeletions incrRemovedModules expos 
       ; tellHtml $ docModulesDiffs expos privs
       }
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
compareSourceFiles new old = do 
    do { tellHtml $ docStartSummary new
       ; pf      <- askParseFun
       ; new_ans <- liftIO $ pf new
       ; old_ans <- liftIO $ pf old
       ; case (new_ans, old_ans) of 
           (Right a, Right b) -> compareModules a b
           (Left err,_)       -> failk (NEW new) err
           (_, Left err)      -> failk (OLD old) err
       }                            
  where 
    failk cmpmod err = do { tellParseFail (fmap sourceFileModule cmpmod)
                          ; tellHtml $ docModuleParseError cmpmod err
                          }


data CompareAlg e = CompareAlg 
       { algName        :: String
       , changedLogger  :: ReportM ()
       , removedLogger  :: ReportM ()
       , diffCollect    :: Module -> Module -> [Edit4 e]
       , textPrinter    :: e -> String
       }


compareModules :: Module -> Module -> ReportM ()
compareModules new old =
    do { runCompareAlg exports_alg    new old 
       ; runCompareAlg datadecls_alg  new old 
       ; runCompareAlg typesigs_alg   new old 
       ; runCompareAlg instances_alg  new old 
       }
 



runCompareAlg :: CompareAlg e -> Module -> Module -> ReportM ()
runCompareAlg alg new old = let diff_list = (diffCollect alg) new old in 
    do { countWarnings (changedLogger alg) (removedLogger alg) diff_list
       ; tellHtml $ 
            renderModifications (algName alg) (textPrinter alg) diff_list
       }

exports_alg :: CompareAlg ExportItem
exports_alg = CompareAlg { algName        = "Exports list"
                         , changedLogger  = incrChangedExports
                         , removedLogger  = incrRemovedExports
                         , diffCollect    = diffExports
                         , textPrinter    = ppExport
                         }
  where
    ppExport (ModuleExport s)  = s
    ppExport (DataOrClass _ s) = s
    ppExport (Variable s)      = s


datadecls_alg :: CompareAlg DatatypeDecl
datadecls_alg = CompareAlg { algName        = "Data declarations"
                           , changedLogger  = incrChangedDatatypes
                           , removedLogger  = incrRemovedDatatypes
                           , diffCollect    = diffDataDecls
                           , textPrinter    = datatype_rep
                           }


typesigs_alg :: CompareAlg TypeSigDecl
typesigs_alg = CompareAlg { algName        = "Type signatures"
                          , changedLogger  = incrChangedTypeSigs
                          , removedLogger  = incrRemovedTypeSigs
                          , diffCollect    = diffTypeSigs
                          , textPrinter    = ppTypeSig 
                          }
  where
    ppTypeSig a = type_decl_name a ++ " :: " ++ type_signature a

instances_alg :: CompareAlg InstanceDecl
instances_alg = CompareAlg { algName        = "Class instances"
                           , changedLogger  = incrChangedInstances
                           , removedLogger  = incrRemovedInstances
                           , diffCollect    = diffInstances
                           , textPrinter    = full_rep
                           }





--------------------------------------------------------------------------------
-- Helpers


countWarnings :: ReportM () -> ReportM () -> [Edit4 a] -> ReportM ()
countWarnings mchange mdelete = mapM_ step where
    step (DIF _ _) = mchange
    step (DEL _)   = mdelete
    step _         = return ()

countDeletions :: ReportM () -> [Edit3 a] -> ReportM ()
countDeletions mf = mapM_ step where
    step (Del _) = mf
    step _       = return ()



renderModifications :: String -> (a -> String) -> [Edit4 a] -> Html
renderModifications txt pp es = case renderBody es of
    [] -> noHtml
    xs -> (h3 << txt) +++ concatHtml xs
  where 
    renderBody (DIF a b:xs) = diffMarkup pp a b : renderBody xs
    renderBody (DEL a:xs)   = delMarkup pp a : renderBody xs 
    renderBody (_:xs)       = renderBody xs
    renderBody []           = []


diffMarkup :: (a -> String) -> a -> a -> Html
diffMarkup pp a b = concatHtml [ p << "New"
                               , pre << pp a
                               , p << "Old"                
                               , pre << pp b
                               ]

delMarkup :: (a -> String) -> a -> Html
delMarkup pp a = concatHtml [ p << "Deleted"
                            , pre << pp a
                            ]
--------------------------------------------------------------------------------
-- Html ...

docHead :: String -> Html
docHead pkg_name = header << doc_title +++ doc_style

  where
    doc_title = thetitle << ("Change summary: " +++ pkg_name)
    doc_style = style ! [thetype "text/css"] << inline_stylesheet

docStartSummary :: SourceFile -> Html
docStartSummary src_file = h2 << (sourceFileModule src_file ++ ":")

docModuleParseError :: CMP SourceFile -> ModuleParseError -> Html
docModuleParseError (OLD _src) err = pre << (moduleParseErrorMsg err)
docModuleParseError (NEW _src) err = pre << (moduleParseErrorMsg err)



docModulesDiffs :: [Edit3 StrName] -> [Edit3 StrName] -> Html
docModulesDiffs expos privs  = expos_doc +++ privs_doc
  where
    expos_doc = maybe docNoExpos (withHeader2 "Exposed modules:")
                      $ modulesTable expos
    privs_doc = maybe docNoPrivs (withHeader2 "Internal modules:")
                      $ modulesTable privs


withHeader2 :: String -> Html -> Html
withHeader2 txt htm = (h2 << txt) +++ htm

docNoExpos :: Html
docNoExpos = p << txt where
    txt = unwords $ 
            [ "No exposed modules counted."
            , "Precis only summarizes libraries or combined"
            , "library/exe packages."
            ]

docNoPrivs :: Html
docNoPrivs = p << txt where
    txt = unwords $ 
            [ "No internal modules counted." ]


modulesTable :: [Edit3 StrName] -> Maybe Html
modulesTable [] = Nothing
modulesTable xs = Just $ table << zipWith fn xs [1::Int ..]
  where
    fn (Add a)   i = makeRow i "+" a
    fn (Equ a)   i = makeRow i ""  a
    fn (Del a)   i = makeRow i "-" a
    
    makeRow i op name = tr << [ td << (show i), td << op, td << name ]


