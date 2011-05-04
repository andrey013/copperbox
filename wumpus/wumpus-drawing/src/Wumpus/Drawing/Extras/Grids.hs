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


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Colour ( black )




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



grid :: (Fractional u, InterpretUnit u) 
     => GridContextF -> (Int,Int) -> (Int,Int) -> Graphic u  
grid upd bl tr = go (upd default_grid_props)
  where
    go props | gp_minor_subdivs props < 1 = gridMajor bl tr props
             | otherwise = gridMinor bl tr props `oplus` gridMajor bl tr props



standard_grid :: GridContextF
standard_grid = id

grid_major_colour :: RGBi -> GridContextF
grid_major_colour rgb = (\s -> s { gp_major_colour = rgb })

grid_major_line_width :: Double -> GridContextF
grid_major_line_width lw = (\s -> s { gp_major_lnwidth = lw })

grid_major_dotnum :: Int -> GridContextF
grid_major_dotnum n = (\s -> s { gp_major_dotnum = n })

grid_minor_subdivisions :: Int -> GridContextF
grid_minor_subdivisions n = (\s -> s { gp_major_dotnum = n })

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



--------------------------------------------------------------------------------



gridMinor :: (Fractional u, InterpretUnit u) 
          => (Int,Int) -> (Int,Int) -> GridProps -> Graphic u
gridMinor bl tr props = 
    let mi    = minorInterior bl tr (gp_minor_subdivs props)
        minF  = lineProps (gp_minor_colour props) (gp_minor_lnwidth props)
                          (gp_minor_dotnum props)
    in localize minF mi

gridMajor :: (Fractional u, InterpretUnit u) 
          => (Int,Int) -> (Int,Int) -> GridProps -> Graphic u
gridMajor bl tr props = 
    let mj    = majorInterior bl tr
        outer = outerRect bl tr
        majF  = lineProps (gp_major_colour props) (gp_major_lnwidth props)
                          (gp_major_dotnum props)        
    in localize majF (mj `oplus` outer)




lineProps :: RGBi -> Double -> Int -> DrawingContextF
lineProps rgb lw n 
    | n < 1     = stroke_colour rgb . set_line_width lw . solid_line 
    | otherwise = stroke_colour rgb . set_line_width lw . dashesF 
  where
    dashesF = set_dash_pattern $ Dash 0 [(1,n)]


-- | Coordinates are expected to be normalized.
--
outerRect :: (Fractional u, InterpretUnit u) 
          => (Int,Int) -> (Int,Int) -> Graphic u  
outerRect cbl@(xmin,ymin) (xmaj,ymaj) = 
    snapmove (xmaj-xmin, ymaj-ymin) >>= \(V2 uw uh) ->
    position cbl                    >>= \bl ->
    dcRectangle STROKE uw uh `at` bl



-- | The major interior is the snap grid. 
--
majorInterior :: (Fractional u, InterpretUnit u)
              => (Int,Int) -> (Int,Int) -> Graphic u
majorInterior cbl@(xmin,ymin) (xmaj,ymaj) = 
    snapmove (xmaj-xmin, ymaj-ymin) >>= \(V2 uw uh) ->
    snapmove (1,1)                  >>= \(V2 w1 h1) ->
    position cbl                    >>= \bl ->
    let xcount = sub1 (xmaj - xmin)
        ycount = sub1 (ymaj - ymin)
        hlines = chainlike ycount (dispV h1) (hline uw)
        vlines = chainlike xcount (dispH w1) (vline uh)
    in         (apply1R1 hlines $ dispV h1 bl)  
       `oplus` (apply1R1 vlines $ dispH w1 bl) 




-- | The minor interior divides each element of the snap grid.
--
minorInterior :: (Fractional u, InterpretUnit u)
              => (Int,Int) -> (Int,Int) -> Int -> Graphic u
minorInterior cbl@(xmin,ymin) (xmaj,ymaj) scount = 
    snapmove (xmaj-xmin, ymaj-ymin) >>= \(V2 uw uh) ->
    snapmove (1,1)                  >>= \(V2 w1 h1) ->
    position cbl                    >>= \bl ->
    let xcount = xmaj - xmin
        ycount = ymaj - ymin
        subw1    = w1 / fromIntegral scount
        subh1    = h1 / fromIntegral scount
        hlines1 = moveStart (dispV subh1) 
                    $ chainlike (scount-1) (dispV subh1) (hline uw)

        vlines1 = moveStart (dispH subw1) 
                    $ chainlike (scount-1) (dispH subw1) (vline uh)
        hlines  = chainlike ycount (dispV h1) hlines1
        vlines  = chainlike xcount (dispH w1) vlines1

    in         (apply1R1 hlines bl)  
       `oplus` (apply1R1 vlines bl) 

-- This doesn\'t work as an advGraphic


-- | This is an operation chain should support, but chain needs a 
-- rethink...
--
chainlike :: Int -> PointDisplace u -> LocGraphic u -> LocGraphic u
chainlike i mv g = promoteR1 $ \start -> go (i-1) (g `at` start) (mv start)
  where
    go n acc pt | n < 1     = acc
                | otherwise = go (n-1) (acc `oplus` (g `at` pt)) (mv pt)
    


vline :: InterpretUnit u => u -> LocGraphic u 
vline len = locStraightLine $ vvec len

hline :: InterpretUnit u => u -> LocGraphic u 
hline len = locStraightLine $ hvec len



sub1 :: Num u => u -> u
sub1 = subtract 1


