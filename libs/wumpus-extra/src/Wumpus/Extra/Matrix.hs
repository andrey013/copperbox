{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Matrix
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Drawing in a matrix grid, inspired by TikZ... 
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Matrix
  (
  
    NodeId
  , GridM       -- opaque

  , Width1
  , Width2
  , Width3
  , Width4
  , Width5
  , Width6
  , Width7
  , Width8
  , Width9
  , Width10

  , nil 
  , node
  , cellpic
  , blank
  , ( & )

  , connect
  
  , cell0
  , cell1
  , cell2
  , cell3
  , cell4
  , cell5
  , cell6
  , cell7
  , cell8
  , cell9
  , cell10

  , runMatrix
  , MatrixProps         -- opaque
  , matrixProps

  ) where

import Wumpus.Core

import Wumpus.Extra.Arrows
import Wumpus.Extra.CoreAdditions ( zeroPicture, bbcenter )
import Wumpus.Extra.PictureLanguage hiding ( blank )
import Wumpus.Extra.Utils

import qualified Data.Map as Map


-- The level naturals
data Z
data S a

type Width1     = S Z
type Width2     = S Width1
type Width3     = S Width2
type Width4     = S Width3
type Width5     = S Width4
type Width6     = S Width5
type Width7     = S Width6
type Width8     = S Width7
type Width9     = S Width8
type Width10    = S Width9

--------------------------------------------------------------------------------

type GridCoord = Point2 Int

type CellValues u = [(GridCoord,CellValue u)]


data CellValue u = CellPic (Picture u)
                 | CellNode SimpleLabel
  deriving (Eq,Show)

type SimpleLabel = String

type NodeId = GridCoord

-- likely to need annotating eg. for arrowheads, curviture...
data Connector = Conn GridCoord GridCoord
  deriving (Eq,Show)

data GridSt = GridSt { posn :: GridCoord, line_count :: Int }
  deriving (Show)

data GridTrace u = GridTrace 
        { cell_values :: CellValues u
        , node_conns  :: [Connector]
        }

trace_zero :: GridTrace u
trace_zero = GridTrace [] []

-- Note - rows always start with nil which increments the y-pos
-- oblivious to its current value. If the initial state was
-- (0,0) the initial nil would move the coord to (0,1), so we
-- have to start the initial coor at (0,-1).
--
grid_state_zero :: GridSt
grid_state_zero = GridSt { posn = P2 0 (-1), line_count = 0 }
  

-- sh - shape
-- u  - unit (usually Double)

data GridM sh u a = GridM { 
          unGridM :: GridSt -> GridTrace u -> (a,GridSt,GridTrace u) }


runGridM :: GridM sh u a -> (a, GridSt, GridTrace u)
runGridM (GridM f) =  f grid_state_zero trace_zero




instance Functor (GridM sh u) where
  fmap f (GridM g) = GridM $ \s w -> let (a,s',w') =  g s w in (f a,s',w')


instance Monad (GridM sh u) where
  return a  = GridM $ \s w -> (a,s,w)
  (GridM f) >>= mf  = GridM $ \s w -> 
    let (a,s',w') = f s w in (unGridM . mf) a s' w'


nextCol :: GridSt -> GridSt
nextCol = pstar upf posn where 
    upf (P2 x y) s = s { posn=(P2 (x+1) y) }


nextRow :: GridSt -> GridSt
nextRow = pstar upf posn where
  upf (P2 _ y) s = s { posn=(P2 0 (y+1)) }

tellNode :: String -> GridCoord -> GridTrace u -> GridTrace u
tellNode name loc = 
    pstar (\xs s -> s { cell_values = (loc,CellNode name):xs }) cell_values

tellPicture :: Picture u -> GridCoord -> GridTrace u -> GridTrace u
tellPicture pic loc = 
    pstar (\xs s -> s { cell_values = (loc,CellPic pic):xs }) cell_values

tellLink :: GridCoord -> GridCoord ->  GridTrace u -> GridTrace u
tellLink a b  = 
    pstar (\xs s -> s { node_conns = Conn a b : xs }) node_conns


--------------------------------------------------------------------------------
-- constructing \'matrices\'


nil :: GridM Z u ()
nil = GridM $ \s w -> ((), nextRow s, w)

blank :: GridM sh u a -> GridM (S sh) u a
blank (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w in (acc, nextCol s', w')


node :: SimpleLabel -> GridM sh u a -> GridM (S sh) u (a,NodeId)
node name (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w 
        loc         = posn s'
    in ((acc,loc), nextCol s', tellNode name loc w')

cellpic :: Picture u -> GridM sh u a -> GridM (S sh) u (a,NodeId)
cellpic pic (GridM mf) = GridM $ \s w -> 
    let (acc,s',w') = mf s w 
        loc         = posn s'
    in ((acc,loc), nextCol s', tellPicture pic loc w')


infixl 5 &
(&) :: GridM sh u a -> (GridM sh u a -> GridM (S sh) u b) -> GridM (S sh) u b
tl & hf = hf tl


connect :: NodeId -> NodeId -> GridM sh u ()
connect from to = GridM $ \s w -> ((),s, tellLink from to w)

--------------------------------------------------------------------------------
-- An arity family of cell \'selectors\'


cell0 :: GridM sh u () -> GridM sh u ()
cell0 (GridM mf) = GridM $ \s w -> mf s w

cell1 :: GridM sh u ((),a) -> GridM sh u a
cell1 (GridM mf) = GridM $ \s w -> 
    let (((),a),s',w') = mf s w in (a,s',w')

cell2 :: GridM sh u (((),a),a) -> GridM sh u (a,a)
cell2 (GridM mf) = GridM $ \s w -> 
    let ((((),a),b),s',w') = mf s w in ((a,b),s',w')

cell3 :: GridM sh u ((((),a),a),a) -> GridM sh u (a,a,a)
cell3 (GridM mf) = GridM $ \s w -> 
    let (((((),a),b),c),s',w') = mf s w in ((a,b,c),s',w')

cell4 :: GridM sh u (((((),a),a),a),a) -> GridM sh u (a,a,a,a)
cell4 (GridM mf) = GridM $ \s w -> 
    let ((((((),a),b),c),d),s',w') = mf s w in ((a,b,c,d),s',w')

cell5 :: GridM sh u ((((((),a),a),a),a),a) -> GridM sh u (a,a,a,a,a)
cell5 (GridM mf) = GridM $ \s w -> 
    let (((((((),a),b),c),d),e),s',w') = mf s w in ((a,b,c,d,e),s',w')

cell6 :: GridM sh u (((((((),a),a),a),a),a),a) -> GridM sh u (a,a,a,a,a,a)
cell6 (GridM mf) = GridM $ \s w -> 
    let ((((((((),a),b),c),d),e),f),s',w') = mf s w in ((a,b,c,d,e,f),s',w')

cell7 :: GridM sh u ((((((((),a),a),a),a),a),a),a) 
      -> GridM sh u (a,a,a,a,a,a,a)
cell7 (GridM mf) = GridM $ \s w -> 
    let (((((((((),a),b),c),d),e),f),g),s',w') = mf s w 
    in ((a,b,c,d,e,f,g),s',w')

cell8 :: GridM sh u (((((((((),a),a),a),a),a),a),a),a) 
      -> GridM sh u (a,a,a,a,a,a,a,a)
cell8 (GridM mf) = GridM $ \s w -> 
    let ((((((((((),a),b),c),d),e),f),g),h),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h),s',w')

cell9 :: GridM sh u ((((((((((),a),a),a),a),a),a),a),a),a)
      -> GridM sh u (a,a,a,a,a,a,a,a,a)
cell9 (GridM mf) = GridM $ \s w -> 
    let (((((((((((),a),b),c),d),e),f),g),h),i),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h,i),s',w')

cell10 :: GridM sh u (((((((((((),a),a),a),a),a),a),a),a),a),a)
      -> GridM sh u (a,a,a,a,a,a,a,a,a,a)
cell10 (GridM mf) = GridM $ \s w -> 
    let ((((((((((((),a),b),c),d),e),f),g),h),i),j),s',w') = mf s w 
    in ((a,b,c,d,e,f,g,h,i,j),s',w')

--------------------------------------------------------------------------------
-- creating a picture


-- Note TikZ shapes (ch. 46 Shape Library) support anchors on 
-- the boundary of shapes, rather than the boundary of the BBox
-- 
-- It would be nice to have shape boundaries...
--
type Border u = BoundingBox u

-- maybe add the scaling function so missing cells can be 'found'...
data BorderMap u = BorderMap 
        { border_table :: Map.Map GridCoord (Border u)
        , fallback_fun :: GridCoord -> Border u
        }


-- Likely more props to follow... 

data MatrixProps u = MatrixProps
      { scaling_vector           :: Vec2 u }


runMatrix :: (Floating u, Fractional u, Real u, Ord u)
          => MatrixProps u -> GridM sh u a -> Picture u
runMatrix props mf = matrixPicture (scaling_vector props) (s,w) 
  where
    (_,s,w) = runGridM mf

matrixProps :: Vec2 u -> MatrixProps u
matrixProps v = MatrixProps { scaling_vector = v }
    


matrixPicture :: (Floating u, Fractional u, Real u, Ord u)
               => Vec2 u -> (GridSt,GridTrace u) -> Picture u
matrixPicture vscale (st,w) = p2
  where
    scalef  = remapCoord st vscale
    (p1,bm) = processCells scalef (cell_values w)
    p2      = processConnectors bm (node_conns w) p1

processCells :: (Floating u, Fractional u, Ord u) 
             => (GridCoord -> Point2 u) 
             -> CellValues u 
             -> (Picture u, BorderMap u)
processCells f = foldr step (zeroPicture,initBorderMap f) 
  where 
    step (coord,CellPic p)  acc     = work coord p acc
    step (coord,CellNode s) acc     = work coord (mkLabel s) acc

    work coord p (ps,bm) = (p' `over` ps, insertBorder coord bb bm)
      where
        p' = p `centeredAt` (f coord) 
        bb = boundary p'

processConnectors :: (Floating u, Real u, Ord u) 
                  => BorderMap u -> [Connector] -> Picture u -> Picture u
processConnectors bm xs p0 = foldr fn p0 xs 
  where
    fn (Conn a b) p = arr `over` p where
      b1  = findBorder a bm
      b2  = findBorder b bm
      arr = arrowTri' () (bbcenter b1) (bbcenter b2)


mkLabel :: (Fractional u, Floating u, Ord u) => String -> Picture u
mkLabel s = frame $ ztextlabel s zeroPt


findBorder :: GridCoord -> BorderMap u -> Border u
findBorder coord bm = maybe fk id $ Map.lookup coord (border_table bm)
  where
    fk = (fallback_fun bm) coord
         

rowCount :: GridSt -> Int 
rowCount (GridSt { posn=(P2 _ h)}) = h

-- /Grid/ coordinates have origin top-left, they are remapped to
-- have the origin at the bottom right.
--
remapCoord :: Num u => GridSt -> (Vec2 u) -> GridCoord -> Point2 u
remapCoord st (V2 sx sy)  (P2 x y) = 
    P2 (sx * fromIntegral x) (sy * fromIntegral (height - y))
  where
    height = rowCount st

initBorderMap :: (GridCoord -> Point2 u) -> BorderMap u
initBorderMap cf = BorderMap { border_table = Map.empty, fallback_fun = fb }
  where
    fb coord = let pt = cf coord in BBox pt pt

insertBorder :: GridCoord -> Border u -> BorderMap u -> BorderMap u
insertBorder coord b =
   pstar (\mp s -> s { border_table = Map.insert coord b mp }) border_table