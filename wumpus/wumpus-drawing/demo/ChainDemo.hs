{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

module ChainDemo where

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Dots.AnchorDots

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_attr chain_pic
    writeEPS "./out/chain_demo.eps" pic1
    writeSVG "./out/chain_demo.svg" pic1


std_attr :: DrawingContext
std_attr = fillColour rosy_brown $ standardContext 6



chain_pic :: DCtxPicture
chain_pic = drawTracing mf 



mf :: (Floating u, FromPtSize u) => TraceDrawing u ()
mf = mapM_ (\pt -> drawi $ dotDisk `at` pt) column_01


-- Note - infinite lists are sometimes /bad/ - okay to zip along 
-- but bad to map on.
-- 
-- A distinction would be good.
--
column_01 :: Num u => [Point2 u]
column_01 = take 10 $ iterate (.+^ vvec (-16)) (P2 0 600)


-- Note hylo fold /after/ unfold.

-- Datorial unfold - c.f "beautiful folding" ? 

data FiniteChain a = forall st. FiniteChain 
      { iter_count :: Int
      , st_zero    :: st
      , gen_step   :: st -> (a, st) 
      }

-- FC1 + adaptor => FC2

data FiniteChain2 a = forall st. FiniteChain2
      { st_zero2   :: st
      , gen_step2  :: st -> Maybe (a,st) 
      }


newtype InfiniteChain a = InfiniteChain { getInfC :: Int -> FiniteChain a }

runFiniteChain :: FiniteChain a -> [a]
runFiniteChain (FiniteChain count st0 gstep) = go count st0
  where
    go n st | n > 0 = let (a,st') = gstep st in a : go (n-1) st'
    go _ _          = [] 

runFiniteChain2 :: FiniteChain2 a -> [a]
runFiniteChain2 (FiniteChain2 st0 gstep) = go $ gstep st0
  where
    go Nothing       = []
    go (Just (a,st)) = a : (go $ gstep st)


adapt :: FiniteChain a -> FiniteChain2 a
adapt (FiniteChain n st0 gstep) = FiniteChain2 (st0,0) astep
  where
    astep (st,i) | i < n = let (a,st') = gstep st in Just (a,(st',i+1))
    astep _              = Nothing

downstep :: Int -> (Int,Int) -> ((Int,Int),(Int,Int))
downstep row_count (x,y) | y == 0 = ((x,0), (x+1,row_count-1)) 
downstep _         (x,y)          = ((x,y), (x,y-1))


tableDownFC :: Int -> Int -> FiniteChain (Int,Int)
tableDownFC rows cols = FiniteChain { iter_count = rows*cols
                                    , st_zero    = (0,rows-1)
                                    , gen_step   = downstep rows }

tableDownFC' :: Int -> Int -> FiniteChain (Int,Int)
tableDownFC' rows cols = makeFC (rows*cols) (0,rows-1) (downstep rows)

tableDownFC2 :: Int -> Int -> FiniteChain2 (Int,Int)
tableDownFC2 rows cols = adapt $ tableDownFC' rows cols


demoFC = runFiniteChain (tableDownFC 3 2)

demoFC' = runFiniteChain2 (tableDownFC2 3 2)


makeFC :: Int -> st -> (st -> (a,st)) -> FiniteChain a
makeFC n st0 gstep = FiniteChain n st0 gstep 

makeFC2 :: st -> (st -> Maybe (a,st)) -> FiniteChain2 a
makeFC2 st0 gstep = FiniteChain2 st0 gstep

 

-- Finite, but could be infinite but then counting is not linear.
--
-- (Also could be done with a list comprehension).
--
tableDown :: Int -> Int -> [(Int,Int)]
tableDown row_count width = stepR 0
   where 
     stepR n | n==width = []
     stepR n            = stepD n rs
     stepD n []         = stepR (n+1)
     stepD n (y:ys)     = (n,y) : stepD n ys
     rs                 = countdown (row_count -1)

countdown :: Int -> [Int]
countdown n | n < 0 = []
countdown n         = n : countdown (n-1)  


univariateX :: Fractional uy => [ux] -> [(ux,uy)]
univariateX xs = zip xs $ iterate (+i) 0
  where
    len = length xs
    i   = rescale 0 1 0 (fromIntegral $ len-1) 1

univariateY :: Fractional ux => [uy] -> [(ux,uy)]
univariateY ys = zip (iterate (+i) 0) ys
  where
    len = length ys
    i   = rescale 0 1 0 (fromIntegral $ len-1) 1

-- count doesn't really help this one as list end already tells
-- us we are done.
--
univariateY_FC :: (Fractional ux, Num uy) => [uy] -> FiniteChain (ux,uy) 
univariateY_FC zs = makeFC len (0,zs) gstep
  where
    gstep (n,[])     = ((n,0),(n,[]))
    gstep (n,y:ys)   = ((n,y),(n+i,ys))  
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1

demoFC2 :: [(Double,Double)]
demoFC2 = runFiniteChain (univariateY_FC [1,2,2,1,2])


univariateY_FC' :: (Fractional ux, Num uy) => [uy] -> FiniteChain2 (ux,uy) 
univariateY_FC' zs = makeFC2 (0,zs) gstep
  where
    gstep (n,[])     = Nothing 
    gstep (n,y:ys)   = Just ((n,y),(n+i,ys))  
    len              = length zs
    i                = rescale 0 1 0 (fromIntegral $ len-1) 1

demoFC2' :: [(Double,Double)]
demoFC2' = runFiniteChain2 (univariateY_FC' [1,2,2,1,2])



rescale :: Fractional a => a -> a -> a -> a -> a -> a
rescale outmin outmax innmin innmax a = 
    outmin + innpos * (outrange / innrange)  
  where
    outrange = outmax - outmin
    innrange = innmax - innmin
    innpos   = a - innmin 

data Projection ux uy u = Projection
      { proj_x  :: ux -> u
      , proj_y  :: uy -> u
      }
  
projectX :: (ux -> u) -> Projection ux u u
projectX xf = Projection { proj_x = xf, proj_y = id }

projectY :: (uy -> u) -> Projection u uy u
projectY yf = Projection { proj_x = id, proj_y = yf }