{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Tree
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
--------------------------------------------------------------------------------

module Wumpus.Tree
  (
  -- * The type of rendered trees
    TreePicture


  -- * Render a Data.Tree to a TreePicture
  , DrawingAttr(..)             -- re-export
  , standardAttr                -- re-export
  , ScaleFactors(..)
  , uniformScaling

  , drawTreePicture

  -- * Output to file
  , writeEPS_TreePicture
  , writeSVG_TreePicture

  -- * Drawing nodes
  , charNode
  , textNode
  , circleNode
  , diskNode

  )
  where

import Wumpus.Tree.Base
import Wumpus.Tree.Design
import Wumpus.Tree.Draw

import Wumpus.Basic.AnchorDots                  -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.Monads.DrawingCtxClass
import Wumpus.Core                              -- package: wumpus-core

import Data.Maybe
import Data.Tree hiding ( drawTree )


writeEPS_TreePicture :: FilePath -> TreePicture -> IO ()
writeEPS_TreePicture = writeEPS_latin1

writeSVG_TreePicture :: FilePath -> TreePicture -> IO ()
writeSVG_TreePicture = writeSVG_latin1

data ScaleFactors = ScaleFactors
      { dx_scale :: Double
      , dy_scale :: Double 
      }
  deriving (Eq,Show)

uniformScaling :: Double -> ScaleFactors
uniformScaling u = ScaleFactors u u 


drawTreePicture :: (a -> TreeNode) -> DrawingAttr -> ScaleFactors -> Tree a -> TreePicture
drawTreePicture drawF attr sfactors tree = 
    fromMaybe errK $ drawGraphic $ drawTree drawF attr $ design funs tree
  where
    funs = scalingFunctions sfactors

errK :: a
errK = error "treePicture - empty tree drawing." 


scalingFunctions :: ScaleFactors -> (Double -> Double, Int -> Double)
scalingFunctions (ScaleFactors sx sy) = (fx,fy)
  where
    fx d = sx * d
    fy d = sy * fromIntegral d

--------------------------------------------------------------------------------
-- Drawing functions

charNode :: Char -> TreeNode
charNode = dotChar

textNode :: String -> TreeNode
textNode = dotText . uptoNewline
  where
    uptoNewline = takeWhile (/='\n')


circleNode :: DRGB -> (a -> TreeNode)
circleNode rgb = const fn
  where
    fn pt = withinModifiedCtx (\s -> s { stroke_colour = rgb}) (dotCircle $ pt)

diskNode :: DRGB -> (a -> TreeNode)
diskNode rgb = const fn
  where
    fn pt = withinModifiedCtx (\s -> s { fill_colour = rgb}) (dotDisk $ pt)



