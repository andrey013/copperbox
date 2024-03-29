{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.HtmlReport
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM concrete syntax for Version 2.0.
-- 
--------------------------------------------------------------------------------

module Wumpus.FontKit.HtmlReport
  ( 

    htmlReport
  , charRow
  , charTable
  , octal
  , boundingbox

  ) where

import Wumpus.FontKit.AfmV2Datatypes    -- TEMP

import Wumpus.Core                              -- package: wumpus-core  
import Wumpus.Basic.Text.Datatypes              -- package: wumpus-basic

import Text.XHtml hiding ( code )               -- package: xhtml

import Numeric




htmlReport :: String -> [(String, Int, CharBBox)] -> Html
htmlReport fontname cs = mkHeader fontname +++ b
  where
    b = body << charTable cs

mkHeader :: String -> Html
mkHeader fontname = header << dtitle +++ dstyle
  where
    dtitle      = thetitle << fontname
    dstyle      = style ! [thetype "text/css"] << inline_stylesheet


-- It is actually convenient to generate a HTML report of the 
-- contents of a font files. SVG viewers do not universally 
-- support scrolling, and Wumpus has only rudimentary support for 
-- multi-page PostScript.
--

charTable :: [(String, Int, CharBBox)] -> Html
charTable xs = table << (tcaption +++ trows)
  where
   tcaption     = caption  << "Characters "
   trows        = tbody    << zipWith charRow [1..] xs  


charRow :: Int -> (String, Int, CharBBox) -> Html
charRow n (charname, code, bb) = 
    tr ! [mkClass n] << [tix, tname, tcode, tcode_oct, tbb]
  where
    tix         = td ! [theclass "ixCol"]   << (stringToHtml $ show n)
    tname       = td ! [theclass "nameCol"] << (stringToHtml $ charname)
    tcode       = td ! [theclass "codeCol"] << (stringToHtml $ show code)
    tcode_oct   = td ! [theclass "octCol"]  << octal code
    tbb         = td ! [theclass "bbCol"]   << boundingbox bb
    
    mkClass i   | odd i         = theclass "odd"
                | otherwise     = theclass "even"

octal :: Int -> Html
octal i | i < 0     = stringToHtml "___" 
        | otherwise = stringToHtml $ '0':'o': step i
  where
    step n | n < 8      = '0' : '0' : show n
           | n < 64     = '0': showOct n ""
           | otherwise  = showOct n ""



boundingbox :: CharBBox -> Html
boundingbox (BBox (P2 llx lly) (P2 urx ury)) = 
    stringToHtml $ ($ "") $ spaceSep $ map afmunitS [llx, lly, urx, ury]
    

--------------------------------------------------------------------------------
-- Strings ...

afmunitS :: AfmUnit -> ShowS
afmunitS = step . realToFrac
  where
    step d  = let d'= tol d in if d == d' then top d else dec d

    tol     :: Double -> Double 
    tol     = realToFrac . roundi 
    
    top d   = showFFloat (Just 0) d 
    
    dec d   = showFFloat (Just 3) d 
     
    roundi  :: Double -> Integer
    roundi  = round

spaceSep :: [ShowS] -> ShowS
spaceSep []     = id
spaceSep [a]    = a
spaceSep (a:as) = a . showChar ' ' . spaceSep as


--------------------------------------------------------------------------------

inline_stylesheet :: Html
inline_stylesheet = primHtml $ unlines $ 
  [ "<!--"
  , "table { "
  , "  border-collapse: collapse;"
  , "}"
  , ""
  , "caption { " 
  , "  text-align: left;"
  , "}"
  , ""
  , ".ixCol {" 
  , "  text-align: right;"
  , "  width: 40px;"
  , "}"
  , ""
  , ".nameCol {" 
  , "  text-align: left;"
  , "  padding-left: 10px;"
  , "  width: 160px;"
  , "}"
  , ""
  , ".codeCol {" 
  , "  text-align: right;"
  , "  width: 50px;"
  , "}"
  , ""
  , ".octCol {" 
  , "  text-align: left;"
  , "  padding-left: 10px;"
  , "  width: 60px;"
  , "}"
  , ""
  , ".bbCol {" 
  , "  text-align: left;"
  , "  padding-left: 10px;"
  , "  width: 160px;"
  , "}"
  , ""
  , ".odd {" 
  , "  background-color: #edf5ff;"
  , "}"
  , ""

  , "-->"
  ]