{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}


module Grid1 where

import Wumpus.Core.Frame
import Wumpus.Core.Instances
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.PostScript
import Wumpus.Core.Transformations
import Wumpus.Core.Vector

import qualified Wumpus.Drawing.Basic as B
import Wumpus.Drawing.BasicCF
import Wumpus.Drawing.Grid
import Wumpus.Drawing.X11Colours

--
import Wumpus.Test.TypeRestrict
import Wumpus.Core.VSExtra
import Wumpus.Core.Matrix

import Data.VectorSpace

demo1 :: IO ()
demo1 = writePS "grid1.ps" $ runWumpus st0 $ drawing1 where
  drawing1 = do { ps_translate 60 380 
                ; B.setRgbColour dodgerBlue1
                ; mapM_ B.drawLine $ grid 20 20 (P2 150 140) origin 
                ---
                ; ps_translate 200 0
                ; mapM_ B.drawLineBag $ sequence calgrid (P2 0 0)
                }
  frame1 = Frame2 (P2 0 0) (V2 1 0) (V2 0 0.5)
  origin = P2 0 0


calgrid :: [LineBag]
calgrid = zipWith fn (calendarGrid 0 30) (repeat dotX) where
  fn :: DVec2 -> LineBag -> LineBag
  fn (V2 x y) df = \o -> map  (fz (x*30) (y*30)) $ df o
  fz x y (LS p p') = LS (translate x y p) (translate x y p')


translatef :: (Floating a, VecMult Matrix3'3 t ) 
           => a -> a -> (b -> t a) -> (b -> t a) 
translatef x y f = \b -> translate x y (f b)