{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.StyleSheet
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


module Precis.StyleSheet
  ( 
    inline_stylesheet

  ) where

import Text.XHtml hiding ( navy, maroon )       -- package: xhtml


inline_stylesheet :: Html
inline_stylesheet = primHtml $ unlines $ 
  [ "<!--"
  , "body { background-color: white; color: black; "
  , "       font-family: sans-serif; padding: 0 0; }"
  , ""
  , "h1    { background-color: " ++ whitesmoke ++ "; color: " ++ brown ++ " }"
  , ".oldcode { background-color: " ++ mintcream ++ "; "
  , "           border: 1px solid #3a3; border-width: 0 1px 0 1px } "
  , ".newcode { background-color: " ++ aliceblue ++ "; "
  , "           border: 1px solid #3a3; border-width: 0 1px 0 1px } "
  , ".deletedcode { background-color: " ++ bisque ++ "; "
  , "               border: 1px solid #3a3; border-width: 0 1px 0 1px } "
  , "-->"
  ]
  
-- some named colours...

aliceblue :: String
aliceblue = "rgb(240,249,255)"

bisque :: String
bisque = "rgb(255,228,197)"

brown :: String 
brown = "rgb(165,43,43)"

-- chocolate :: String
-- chocolate = "rgb(211,106,31)"

-- crimson :: String
-- crimson = "rgb(221,20,60)"

-- indianred :: String
-- indianred = "rgb(206,93,93)"

-- lightcoral :: String
-- lightcoral = "rgb(240,128,128)"

-- lightsalmon :: String
-- lightsalmon = "rgb(255,160,122)"

-- maroon :: String
-- maroon = "rgb(128,0,0)"

mintcream :: String
mintcream = "rgb(246,255,250)"

-- navy :: String 
-- navy = "rgb(0,0,128)"

whitesmoke :: String
whitesmoke = "rgb(246,246,246)"