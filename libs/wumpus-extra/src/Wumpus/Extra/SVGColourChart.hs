{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.SVGColourChart
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- All the SVG \'named colours\' in a table.
--
--------------------------------------------------------------------------------

module Wumpus.Extra.SVGColourChart
  (
  -- * All SVG colours in a table
    all_svg_colours
  
  ) where

import Wumpus.Extra.SVGColours

import Wumpus.Core.Colour ( RGB3(..), DRGB )


import Prelude hiding ( tan )
  


all_svg_colours :: [(String,DRGB)]
all_svg_colours = 
  [ ("aliceBlue",               aliceBlue)
  , ("antiqueWhite",            antiqueWhite)
  , ("aqua",                    aqua)
  , ("aquamarine",              aquamarine)
  , ("azure",                   azure)
  , ("beige",                   beige)
  , ("bisque",                  bisque)
  , ("black",                   black)
  , ("blanchedAlmond",          blanchedAlmond)
  , ("blue",                    blue)
  , ("blueViolet",              blueViolet)
  , ("brown",                   brown)
  , ("burlywood",               burlywood)
  , ("cadetBlue",               cadetBlue)
  , ("chartreuse",              chartreuse)
  , ("chocolate",               chocolate)
  , ("coral",                   coral)
  , ("cornflowerBlue",          cornflowerBlue)
  , ("cornsilk",                cornsilk)
  , ("crimson",                 crimson)
  , ("cyan",                    cyan)
  , ("darkBlue",                darkBlue)
  , ("darkCyan",                darkCyan)
  , ("darkGoldenrod",           darkGoldenrod)
  , ("darkGray",                darkGray)
  , ("darkGreen",               darkGreen)
  , ("darkGrey",                darkGrey)
  , ("darkKhaki",               darkKhaki)
  , ("darkMagenta",             darkMagenta)
  , ("darkOliveGreen",          darkOliveGreen)
  , ("darkOrange",              darkOrange)
  , ("darkOrchid",              darkOrchid)
  , ("darkRed",                 darkRed)
  , ("darkSalmon",              darkSalmon)
  , ("darkSeaGreen",            darkSeaGreen)
  , ("darkSlateBlue",           darkSlateBlue)
  , ("darkSlateGray",           darkSlateGray)
  , ("darkSlateGrey",           darkSlateGrey)
  , ("darkTurquoise",           darkTurquoise)
  , ("darkViolet",              darkViolet)
  , ("deepPink",                deepPink)
  , ("deepSkyBlue",             deepSkyBlue)
  , ("dimGray",                 dimGray)
  , ("dimGrey",                 dimGrey)
  , ("dodgerBlue",              dodgerBlue)
  , ("firebrick",               firebrick)
  , ("floralWhite",             floralWhite)
  , ("forestGreen",             forestGreen)
  , ("fuchsia",                 fuchsia)
  , ("gainsboro",               gainsboro)
  , ("ghostWhite",              ghostWhite)
  , ("gold",                    gold)
  , ("goldenrod",               goldenrod)
  , ("gray",                    gray)
  , ("grey",                    grey)
  , ("green",                   green)
  , ("greenYellow",             greenYellow)
  , ("honeydew",                honeydew)
  , ("hotPink",                 hotPink)
  , ("indianRed",               indianRed)
  , ("indigo",                  indigo)
  , ("ivory",                   ivory)
  , ("khaki",                   khaki)
  , ("lavender",                lavender)
  , ("lavenderBlush",           lavenderBlush)
  , ("lawnGreen",               lawnGreen)
  , ("lemonChiffon",            lemonChiffon)
  , ("lightBlue",               lightBlue)
  , ("lightCoral",              lightCoral)
  , ("lightCyan",               lightCyan)
  , ("lightGoldenrodYellow",    lightGoldenrodYellow)
  , ("lightGray",               lightGray)
  , ("lightGreen",              lightGreen)
  , ("lightGrey",               lightGrey)
  , ("lightPink",               lightPink)
  , ("lightSalmon",             lightSalmon)
  , ("lightSeaGreen",           lightSeaGreen)
  , ("lightSkyBlue",            lightSkyBlue)
  , ("lightSlateGray",          lightSlateGray)
  , ("lightSlateGrey",          lightSlateGrey)
  , ("lightSteelBlue",          lightSteelBlue)
  , ("lightYellow",             lightYellow)
  , ("lime",                    lime)
  , ("limeGreen",               limeGreen)
  , ("linen",                   linen)
  , ("magenta",                 magenta)
  , ("maroon",                  maroon)
  , ("mediumAquamarine",        mediumAquamarine)
  , ("mediumBlue",              mediumBlue)
  , ("mediumOrchid",            mediumOrchid)
  , ("mediumPurple",            mediumPurple)
  , ("mediumSeaGreen",          mediumSeaGreen)
  , ("mediumSlateBlue",         mediumSlateBlue)
  , ("mediumSpringGreen",       mediumSpringGreen)
  , ("mediumTurquoise",         mediumTurquoise)
  , ("mediumVioletRed",         mediumVioletRed)
  , ("midnightBlue",            midnightBlue)
  , ("mintcream",               mintcream)
  , ("mistyrose",               mistyrose)
  , ("moccasin",                moccasin)
  , ("navajoWhite",             navajoWhite)
  , ("navy",                    navy)
  , ("oldlace",                 oldlace)
  , ("olive",                   olive)
  , ("oliveDrab",               oliveDrab)
  , ("orange",                  orange)
  , ("orangeRed",               orangeRed)
  , ("orchid",                  orchid)
  , ("paleGoldenrod",           paleGoldenrod)
  , ("paleGreen",               paleGreen)
  , ("paleTurquoise",           paleTurquoise)
  , ("paleVioletRed",           paleVioletRed)
  , ("papayawhip",              papayawhip)
  , ("peachpuff",               peachpuff)
  , ("peru",                    peru)
  , ("pink",                    pink)
  , ("plum",                    plum)
  , ("powderBlue",              powderBlue)
  , ("purple",                  purple)
  , ("red",                     red)
  , ("rosyBrown",               rosyBrown)
  , ("royalBlue",               royalBlue)
  , ("saddleBrown",             saddleBrown)
  , ("salmon",                  salmon)
  , ("sandyBrown",              sandyBrown)
  , ("seaGreen",                seaGreen)
  , ("seashell",                seashell)
  , ("sienna",                  sienna)
  , ("silver",                  silver)
  , ("skyBlue",                 skyBlue)
  , ("slateBlue",               slateBlue)
  , ("slateGray",               slateGray)
  , ("slateGrey",               slateGrey)
  , ("snow",                    snow)
  , ("springGreen",             springGreen)
  , ("steelBlue",               steelBlue)
  , ("tan",                     tan)
  , ("teal",                    teal)
  , ("thistle",                 thistle)
  , ("tomato",                  tomato)
  , ("turquoise",               turquoise)
  , ("violet",                  violet)
  , ("wheat",                   wheat)
  , ("white",                   white)
  , ("whitesmoke",              whitesmoke)
  , ("yellow",                  yellow)
  , ("yellowGreen",             yellowGreen)
  ]

