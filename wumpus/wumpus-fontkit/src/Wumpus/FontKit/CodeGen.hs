{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.CodeGen
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Utility building blocks for code generation.
--
--------------------------------------------------------------------------------

module Wumpus.FontKit.CodeGen
  ( 
    wallDirective
  , haddockModuleBlock
  , moduleDecl
  , haddockComment
  , importList

  , lhspre
  , pairList

  , yearName

  ) where


import Wumpus.Basic.Utils.FormatCombinators

import Data.Time


wallDirective :: Doc 
wallDirective = text "{-# OPTIONS -Wall #-}"


haddockModuleBlock :: String -> String -> [String] -> ZonedTime -> Doc
haddockModuleBlock qmodname year_string descr_body ztime = 
    vcat [ dashLineSep, startHaddock empty
         , modu_line, copyright, license
         , contHaddock empty
         , maintainer, stability, portability
         , contHaddock empty
         , vcat $ map (contHaddock . text) descr_body
         , contHaddock empty
         , contHaddock (text "Generated -" <+> timeStamp ztime) 
         , contHaddock empty, dashLineSep ]
  where
    modu_line   = haddockModuleProp "Module" qmodname
    copyright   = haddockModuleProp "Copyright" 
                                    ("(c) Stephen Tetley " ++ year_string)
    license     = haddockModuleProp "License" "BSD3"
    maintainer  = haddockModuleProp "Maintainer" 
                                    "Stephen Tetley <stephen.tetley@gmail.com>"
    stability   = haddockModuleProp "Stability" "unstable"
    portability = haddockModuleProp "Portability" "GHC"

    

moduleDecl :: String -> [String] -> Doc
moduleDecl qmodname exports = 
    vcat [ modu_prefix, open_paren, exportList exports, modu_suffix ]
  where
    modu_prefix = lhspre $ text "module" <+> text qmodname
    open_paren  = lhspre $ text "  ("
    modu_suffix = lhspre $ text "  ) where"

exportList :: [String] -> Doc
exportList [] = empty
exportList (e:es) = vcat $ (lhspre $ text "    " <+> text e) : rest
  where
    rest = map (\s -> lhspre $ text "  ," <+> text s) es


importList :: [String] -> Doc
importList = foldr fn empty
  where
    fn [] acc = empty `vconcat` acc
    fn ss acc = (lhspre $ text "import" <+> text ss) `vconcat` acc


lhspre :: Doc -> Doc
lhspre d = char '>' <+> d

haddockComment :: [String] -> Doc
haddockComment []     = empty
haddockComment (s:ss) = 
    vcat $ startHaddock (text s) : map (contHaddock . text) ss 

startHaddock :: Doc -> Doc
startHaddock d = lhspre $ text "-- |" <+> d

contHaddock :: Doc -> Doc
contHaddock d = lhspre $ text "--" <+> d


dashLineSep :: Doc
dashLineSep = lhspre $ text (replicate 78 '-')

haddockModuleProp :: String -> String -> Doc
haddockModuleProp name val = contHaddock $ 
   fillStringR 12 name <+> char ':' <+> space <+> text val


pairList :: (a -> Doc) -> (b -> Doc) -> [(a,b)] -> Doc
pairList _ _ []         = lhspre $ text "    [ ]"
pairList f g (x:xs) = 
    vcat [ first_elem,  rest_elems, lhspre $ text "    ]" ]
  where
    first_elem = lhspre $ text "    [" <+> fn x
    rest_elems = vcat $ map (\a -> lhspre $ text "    ," <+> fn a) xs

    fn (a,b)   = lparen <+> f a <> comma <+> g b <+> rparen



timeStamp :: ZonedTime -> Doc
timeStamp zt = local_day <+> local_time
  where
    loc_time    = zonedTimeToLocalTime zt
    local_time  = timeOfDay             $ localTimeOfDay loc_time
    local_day   = text $ showGregorian  $ localDay loc_time

timeOfDay :: TimeOfDay -> Doc
timeOfDay t = 
    (pad2 $ todHour t) <> char ':' <> (pad2 $ todMin t)
  where
  
    pad2 :: Int -> Doc
    pad2 i | i < 10    = char '0' <> text (show i)
           | otherwise = text (show i)  


yearName :: ZonedTime -> String
yearName zt = show yr
  where 
    (yr,_,_) = toGregorian $ localDay $ zonedTimeToLocalTime zt

