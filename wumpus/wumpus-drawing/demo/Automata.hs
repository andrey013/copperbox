{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}



module Automata where

import Wumpus.Drawing.Arrows
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Paths
import Wumpus.Drawing.Shapes
import Wumpus.Drawing.Text.SafeFonts
import Wumpus.Drawing.Text.LRText

import FontLoaderUtils

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import System.Directory




main :: IO ()
main = do 
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    maybe gs_failk  makeGSPicture  $ mb_gs
    maybe afm_failk makeAfmPicture $ mb_afm
  where
    gs_failk  = putStrLn "No GhostScript font path supplied..."
    afm_failk = putStrLn "No AFM v4.1 font path supplied..."

makeGSPicture :: FilePath -> IO ()
makeGSPicture font_dir = do 
    putStrLn "Using GhostScript metrics..."
    (base_metrics, msgs) <- loadGSMetrics font_dir automata_fonts 
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata01.eps" pic1
    writeSVG "./out/automata01.svg" pic1 

makeAfmPicture :: FilePath -> IO ()
makeAfmPicture font_dir = do 
    putStrLn "Using AFM 4.1 metrics..."
    (base_metrics, msgs) <- loadAfmMetrics font_dir automata_fonts
    mapM_ putStrLn msgs
    let pic1 = runCtxPictureU (makeCtx base_metrics) automata
    writeEPS "./out/automata02.eps" pic1
    writeSVG "./out/automata02.svg" pic1 

automata_fonts :: [FontName]
automata_fonts = map ps_font_name [ times_roman, times_italic ]

makeCtx :: GlyphMetrics -> DrawingContext
makeCtx = snapGrid 60.0 60.0 . fontFace helvetica . metricsContext 14


automata :: DCtxPicture
automata = drawTracing $ do
    q0     <- starti   $ state
    q1     <- relativei  (above_right q0)      $ state
    q2     <- relativei  (below_right q0)      $ state
    q3     <- relativei  (below_right q1)      $ state

    s0     <- query (left_of q0)
    
    drawcr q0 q1 $ straightconn
    drawcr q1 q3 $ straightconn
    drawcr q0 q2 $ straightconn
    drawcr q2 q3 $ straightconn
    drawc  s0 (west q0) $ straightconn

    return ()


state :: ( Real u, Floating u, FromPtSize u) 
      => LocImage u (Circle u)
state = strokedShape $ circle 20


straightconn :: (Real u, Floating u, FromPtSize u) 
             => ConnectorGraphic u
straightconn = ignoreAns $ rightArrow tri45 connLine





-- ignoreAns $ rightArrow tri45 connLine


-----------------------

-- Candidates for library modules...

--  drawZero ? drawZeroi ?  drawZeroi_ ?



start :: (Fractional u, TraceM m, DrawingCtxM m, u ~ DUnit (m ())) 
      => LocGraphic u -> m ()
start gf =  askDC          >>= \ctx -> 
            let (_,o) = runCF1 ctx zeroPt gf in trace (collectH o)

starti :: (Fractional u, TraceM m, DrawingCtxM m, u ~ DUnit (m ())) 
       => LocImage u a -> m a
starti gf =  askDC          >>= \ctx -> 
             let (a,o) = runCF1 ctx zeroPt gf in trace (collectH o) >> return a

starti_ :: (Fractional u, TraceM m, DrawingCtxM m, u ~ DUnit (m ())) 
        => LocImage u a -> m ()
starti_ gf = starti gf >> return ()


--  drawCtx ? drawCtxi ?  drawCtxi_ ?


relative :: (Fractional u, TraceM m, DrawingCtxM m, u ~ DUnit (m ())) 
         => DrawingInfo (Point2 u) -> LocGraphic u -> m ()
relative pf gf = 
    askDC  >>= \ctx -> let pt    = runCF  ctx pf
                           (_,o) = runCF1 ctx pt gf 
                       in trace (collectH o)


relativei :: (Fractional u, TraceM m, DrawingCtxM m, u ~ DUnit (m ())) 
       => DrawingInfo (Point2 u) -> LocImage u a -> m a
relativei pf gf =  
    askDC  >>= \ctx -> let pt    = runCF  ctx pf
                           (a,o) = runCF1 ctx pt gf 
                       in trace (collectH o) >> return a

relativei_ :: (Fractional u, TraceM m, DrawingCtxM m, u ~ DUnit (m ())) 
        => DrawingInfo (Point2 u) -> LocImage u a -> m ()
relativei_ pf gf = relativei pf gf >> return ()

-- drawRadCon

drawcr :: ( Real u, Floating u, FromPtSize u
          , CenterAnchor t1, RadialAnchor  t1
          , CenterAnchor t2, RadialAnchor  t2
          , DrawingCtxM m,   TraceM m
          , u ~ DUnit t1,  DUnit t1 ~ DUnit t2, u ~ DUnit (m ()) ) 
       => t1 -> t2 -> ConnectorGraphic  u -> m ()
drawcr a b gf = let (p0,p1) = radialConnectorPoints a b 
                in draw (connect gf p0 p1)


centerRelative :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
               => (Int,Int) -> a -> DrawingInfo (Point2 u)
centerRelative coord a =
    let pt = center a in sgmove coord >>= \v -> return (pt .+^ v)


right_of        :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
                => a -> DrawingInfo (Point2 u)
right_of        = centerRelative (1,0)

left_of         :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
                => a -> DrawingInfo (Point2 u)
left_of         = centerRelative ((-1),0)

above_right     :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
                => a -> DrawingInfo (Point2 u)
above_right     = centerRelative (1,1)

below_right     :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
                => a -> DrawingInfo (Point2 u)
below_right     = centerRelative (1, (-1))

above_left      :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
                => a -> DrawingInfo (Point2 u)
above_left      = centerRelative ((-1),1)

below_left      :: (CenterAnchor a, Fractional u, u ~ DUnit a) 
                => a -> DrawingInfo (Point2 u)
below_left      = centerRelative ((-1),(-1))
 




sgmove :: (Fractional u, DrawingCtxM m) 
       => (Int,Int) -> m (Vec2 u)
sgmove (x,y) = fn <$> asksDC snap_grid_factors
  where
    fn (scx,scy) = V2 ((realToFrac scx) * fromIntegral x)
                      ((realToFrac scy) * fromIntegral y)


-- | 'snapGrid' : @ x_unit * y_unit -> DrawingContextF @
--
snapGrid   :: Double -> Double -> DrawingContextF
snapGrid xu yu = (\s -> s { snap_grid_factors = (xu,yu) })






