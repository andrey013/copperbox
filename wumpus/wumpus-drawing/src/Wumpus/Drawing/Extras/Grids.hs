{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Extras.Grids
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Drawing grids
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Extras.Grids
  ( 
   
    GridContextF
  , grid
  , standard_grid
  , dotted_major_grid

  , grid_major_colour
  , grid_major_line_width
  , grid_major_dotnum
  , grid_minor_subdivisions
  , grid_minor_colour
  , grid_minor_line_width
  , grid_minor_dotnum
  , grid_point_size
  , grid_label_colour

  ) where


import Wumpus.Drawing.Basis.DrawingPrimitives

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Data.Monoid


type GridContextF = GridProps -> GridProps

-- | GridProps control the drawing of grids.
-- 
data GridProps = GridProps
      { gp_major_colour   :: RGBi
      , gp_major_lnwidth  :: Double
      , gp_major_dotnum   :: Int
      , gp_minor_subdivs  :: Int
      , gp_minor_colour   :: RGBi
      , gp_minor_lnwidth  :: Double
      , gp_minor_dotnum   :: Int
      , gp_point_size     :: FontSize
      , gp_label_colour   :: RGBi
      }

default_grid_props :: GridProps
default_grid_props = 
    GridProps { gp_major_colour     = grey1
              , gp_major_lnwidth    = 1
              , gp_major_dotnum     = 0
              , gp_minor_subdivs    = 5
              , gp_minor_colour     = grey2
              , gp_minor_lnwidth    = 0.5
              , gp_minor_dotnum     = 0
              , gp_point_size       = 10
              , gp_label_colour     = black
              }
  where
    grey1 = RGBi 100 100 100
    grey2 = RGBi 150 150 150 




standard_grid :: GridContextF
standard_grid = id

dotted_major_grid :: GridContextF
dotted_major_grid = 
    grid_minor_subdivisions 0 . grid_major_dotnum 2

-- Setters for client code.

grid_major_colour :: RGBi -> GridContextF
grid_major_colour rgb = (\s -> s { gp_major_colour = rgb })

grid_major_line_width :: Double -> GridContextF
grid_major_line_width lw = (\s -> s { gp_major_lnwidth = lw })

grid_major_dotnum :: Int -> GridContextF
grid_major_dotnum n = (\s -> s { gp_major_dotnum = n })

grid_minor_subdivisions :: Int -> GridContextF
grid_minor_subdivisions n = (\s -> s { gp_minor_subdivs = n })

grid_minor_colour :: RGBi -> GridContextF
grid_minor_colour rgb = (\s -> s { gp_minor_colour = rgb })

grid_minor_line_width :: Double -> GridContextF
grid_minor_line_width lw = (\s -> s { gp_minor_lnwidth = lw })

grid_minor_dotnum :: Int -> GridContextF
grid_minor_dotnum n = (\s -> s { gp_minor_dotnum = n })


grid_point_size :: FontSize -> GridContextF
grid_point_size i = (\s -> s { gp_point_size = i })


grid_label_colour :: RGBi -> GridContextF
grid_label_colour rgb = (\s -> s { gp_label_colour = rgb })




-- Drawing context updaters...

major_line_update :: GridProps -> DrawingContextF
major_line_update (GridProps { gp_major_colour  = rgb
                             , gp_major_lnwidth = lnwidth
                             , gp_major_dotnum  = dotnum }) = 
    lineProps rgb lnwidth dotnum 

minor_line_update :: GridProps -> DrawingContextF
minor_line_update (GridProps { gp_minor_colour  = rgb
                             , gp_minor_lnwidth = lnwidth
                             , gp_minor_dotnum  = dotnum }) = 
    lineProps rgb lnwidth dotnum 


lineProps :: RGBi -> Double -> Int -> DrawingContextF
lineProps rgb lw n 
    | n < 1     = stroke_colour rgb . set_line_width lw . solid_line 
    | otherwise = stroke_colour rgb . set_line_width lw . dashesF 
  where
    dashesF = set_dash_pattern $ Dash 0 [(1,n)]



--------------------------------------------------------------------------------


grid :: (Fractional u, InterpretUnit u) 
     => GridContextF -> Int -> Int -> LocGraphic u  
grid upd nx ny = 
    snapmove (1,1) >>= \(V2 uw uh) ->
    let props  = upd default_grid_props
        width  = uw * fromIntegral nx
        height = uh * fromIntegral ny
        intrr  = gridInterior nx width uw ny height uh props
        rect   = localize (major_line_update props) $ 
                   blRectangle STROKE width height
    in intrr `mappend` rect

                 

gridInterior :: (Fractional u, InterpretUnit u) 
             => Int -> u -> u -> Int -> u -> u -> GridProps -> LocGraphic u
gridInterior nx w uw ny h uh props = hlines `mappend` vlines
  where
    hlines = horizontalLines ny w uh props
    vlines = verticalLines   nx h uw props


horizontalLines :: (Fractional u, InterpretUnit u) 
                => Int -> u -> u -> GridProps -> LocGraphic u
horizontalLines numh w uh props@(GridProps { gp_minor_subdivs = subs })
    | subs > 0  = let dy = uh / (fromIntegral subs)
                      n  = (numh * subs) - 1
                  in moveStart (vvec dy) $ minorMajor n subs (vvec dy) mnr mjr
    | otherwise = moveStart (vvec uh) $ duplicate numh (vvec uh) mjr
  where
    mnr  = localize (minor_line_update props) $ hline w
    mjr  = localize (major_line_update props) $ hline w



verticalLines :: (Fractional u, InterpretUnit u) 
              => Int -> u -> u -> GridProps -> LocGraphic u
verticalLines numv h uw props@(GridProps { gp_minor_subdivs = subs })
    | subs > 0  = let dx = uw / (fromIntegral subs)
                      n  = (numv * subs) - 1
                  in moveStart (hvec dx) $ minorMajor n subs (hvec dx) mnr mjr
    | otherwise = moveStart (hvec uw) $ duplicate numv (hvec uw) mjr
  where
    mnr  = localize (minor_line_update props) $ vline h
    mjr  = localize (major_line_update props) $ vline h




minorMajor :: InterpretUnit u 
           => Int -> Int -> Vec2 u -> LocGraphic u -> LocGraphic u 
           -> LocGraphic u
minorMajor count alt mv mnr mjr = runLocTrace (step count)
  where
    step n | n <= 0           = ureturn
           | n `mod` alt == 0 = insertl mjr >> moveby mv >> step (n-1)
           | otherwise        = insertl mnr >> moveby mv >> step (n-1)
 


