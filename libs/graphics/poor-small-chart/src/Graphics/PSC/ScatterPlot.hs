{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.ScatterPlot
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Scatter plots...
-- 
--------------------------------------------------------------------------------

module Graphics.PSC.ScatterPlot
 where

import Wumpus.Core


type ScatterPlot = DPicture


writeScatterPlotEPS :: FilePath -> ScatterPlot -> IO ()
writeScatterPlotEPS = writeEPS_latin1 


writeScatterPlotSVG :: FilePath -> ScatterPlot -> IO ()
writeScatterPlotSVG = writeSVG_latin1 


drawScatterPlot :: DRGB -> [(Double,Double)] -> ScatterPlot
drawScatterPlot rgb = frameMulti . plot rgb

plot :: DRGB -> [(Double,Double)] -> [DPrimitive]
plot rgb = map fn where
    fn (x,y) = ellipse rgb 2 2 (P2 x y)