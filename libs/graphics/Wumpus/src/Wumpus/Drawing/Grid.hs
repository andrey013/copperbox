{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Grid
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Grids and /addressable/ matrices.
--
--------------------------------------------------------------------------------


module Wumpus.Drawing.Grid where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Frame
import Wumpus.Core.Fun
import Wumpus.Core.Line
import Wumpus.Core.Point
import Wumpus.Core.Vector

import Wumpus.Drawing.Picture

import MonadLib.Monads

type Row = Int
type Col = Int

type Idx = (Row,Col)

data Node = Node Idx String
  deriving (Show)

data Empty = Empty


push :: Monad m => m a -> s -> m (s,a)
push ma s = ma >>= \a -> return (s,a)

begin :: Monad m => m Empty
begin = return Empty


mkNode :: String -> State Idx Node
mkNode str = get >>= \ix -> return $ Node ix str


matrix :: State Idx a -> (a,Idx)
matrix = runState (1,1)

row :: (Empty -> State Idx b) -> State Idx b
row = (begin .&.)



infixr 9 $$
($$) :: (a -> b) -> a -> b
($$) = ($)

row1 :: State Idx (Empty,a) -> State Idx a
row2 :: State Idx ((Empty,a),b) -> State Idx (a,b)
row3 :: State Idx (((Empty,a),b),c) -> State Idx (a,b,c)

row1 mstk = mstk >>= \(Empty,a) -> return a
row2 mstk = mstk >>= \((Empty,a),b) -> return (a,b)
row3 mstk = mstk >>= \(((Empty,a),b),c) -> return (a,b,c)

infixl 8 .&.
(.&.) :: State Idx stk -> (stk -> State Idx stk') -> State Idx stk'
(.&.) = (>>=)


node :: String -> stk -> State Idx (stk,Node)
node s  = push (mkNode s)


blank :: stk -> State Idx stk
blank = return




{-

grid :: Double -> Double -> Double -> Double -> Picture
grid xstep ystep w h = withFrame $ \frm -> 
    fork (mapM_ drawLine, bounds) (gridlines frm)
  where
    xpoints pt@(P2 x0 y) = genPoints (\(P2 x _) -> x <~= (x0+w))
                                     (\(P2 x _) -> P2 (x+xstep) y)
                                     pt
    ypoints pt@(P2 x y0) = genPoints (\(P2 _ y) -> y <~= (y0+h))
                                     (\(P2 _ y) -> P2 x (y+ystep))
                                     pt
    hlines ogin = map (hline w) $ ypoints ogin
    vlines ogin = map (vline h) $ xpoints ogin

    gridlines frm = let pt = withinFrame frm zeroPt in hlines pt ++ vlines pt     


vgrid :: Num a => (Integer,Integer) -> (Integer,Integer) -> [Vec2 a]
vgrid (i,j) (i',j') = [ mkvec a b | b <- [j..j']
                                  , a <- [i..i'] ]
  where
    mkvec a b = V2 (fromInteger a) (fromInteger b)

calendarGrid :: Num a => Int -> Int -> [Vec2 a]
calendarGrid st n = map transp $ take n $ drop st $  vgrid (0,0) (6,5)
  where
    transp (V2 x y) = (V2 x (tot-y))
    tot  = let (a,b) = (fromIntegral n) `divMod` 7 in 
           if (st+b >7) then (fromIntegral $ a+1) else (fromIntegral a)

-}