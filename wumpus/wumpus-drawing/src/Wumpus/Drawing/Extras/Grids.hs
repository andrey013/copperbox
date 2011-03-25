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
    Grid
  , GridContextF
  , grid
  , evalGrid
  , localizeGrid
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



newtype Grid u = Grid { getGrid :: GridProps -> Graphic u }

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
     => (Int,Int) -> (Int,Int) -> Grid u  
grid bl tr = Grid $ \props -> 
    if gp_minor_subdivs props < 1 
       then gridMajor bl tr props
       else gridMinor bl tr props `oplus` gridMajor bl tr props




evalGrid :: Grid u -> Graphic u
evalGrid gf = getGrid gf default_grid_props 

localizeGrid :: GridContextF -> Grid u -> Grid u 
localizeGrid upd gf = Grid $ \props -> getGrid gf (upd props)


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
    strokedRectangle uw uh `at` bl



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
        hlines = chainlike ycount (displaceV h1) (hline uw)
        vlines = chainlike xcount (displaceH w1) (vline uh)
    in         (apply1R1 hlines $ displaceV h1 bl)  
       `oplus` (apply1R1 vlines $ displaceH w1 bl) 




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
        hlines1 = moveStart (displaceV subh1) 
                    $ chainlike (scount-1) (displaceV subh1) (hline uw)

        vlines1 = moveStart (displaceH subw1) 
                    $ chainlike (scount-1) (displaceH subw1) (vline uh)
        hlines  = chainlike ycount (displaceV h1) (ignoreAns hlines1)
        vlines  = chainlike xcount (displaceH w1) (ignoreAns vlines1)

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


{-
advVLine :: InterpretUnit u => u -> u -> AdvGraphic u 
advVLine len space = 
    intoAdvGraphic (return $ hvec space) (locStraightLine $ vvec len)

advHLine :: InterpretUnit u => u -> u ->  AdvGraphic u 
advHLine len space = 
    intoAdvGraphic (return $ vvec space) (locStraightLine $ hvec len)

-- FOR WUMPUS_BASIC...
--
-- | Repeat the graphic @n@ times concatenating the result.
--
advtimes :: InterpretUnit u => Int -> AdvGraphic u -> AdvGraphic u
advtimes n = advconcat . replicate n
-}

--------------------------------------------------------------------------------

{-
-- | Note - the grid is originated at whatever implicit start
-- point is used. It is not snapped to /nice round/ numbers.
-- 
grid :: (Fractional u, InterpretUnit u)
     => (Int,Int) -> RGBi -> LocGraphic u
grid (nx,ny) rgb    
    | nx < 1 || ny < 1 = emptyLocGraphic
    | otherwise        = localize (stroke_colour rgb) $ 
        lift0R1 snapGridFactors >>= \(x_incr, y_incr) ->
        let rectw  = x_incr * fromIntegral nx
            recth  = y_incr * fromIntegral ny
            grid1  = interiorGrid x_incr nx y_incr ny rectw recth
        in grid1 `oplus` strokedRectangle rectw recth



-- | Get the Point corresponding the grid coordinates scaled by
-- the snap-grid scaling factors.
--
snapGridFactors :: (Fractional u, InterpretUnit u) =>  Query (u,u)
snapGridFactors = (\(V2 x y) -> (x,y)) <$> snapmove (1,1)


-- | 'interiorGrid' : @ increment -> ConnectorGraphic @
--
-- Draw the interior lines of a grid between the /connector/ 
-- points - start point is interpreted as bottom-left, end-point
-- is interpreted as top right.
--
-- The interior lines are calculated with repsect to the 0 and the 
-- increment, for instance with an increment of 10 but a start 
-- point @(15,0)@ lines are drawn from @(20,0), (30,0)@ etc.
--
interiorGrid :: InterpretUnit u
             => u -> Int -> u -> Int -> u -> u -> LocGraphic u
interiorGrid x_step nx y_step ny w h = hlines `oplus` vlines
  where
    hline1 = locStraightLine (hvec w)
    vline1 = locStraightLine (vvec h)
    vlines = ignoreAns $ moveStart (displaceH x_step) $
               chainH x_step (replicate (nx-1) vline1) 
    hlines = ignoreAns $ moveStart (displaceV y_step) $ 
               chainV y_step (replicate (ny-1) hline1) 

-}