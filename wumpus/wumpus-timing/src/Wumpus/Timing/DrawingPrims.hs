{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.DrawingPrims
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Timing.DrawingPrims
  (

    Props(..)
  , defaultProps
  , Prefix(..)

  , lineHigh
  , lineLow
  , lineMid

  , glitch
    
  , fillCamferedRect
  , fillRightCamferedRect

  , Metastasis(..)
  , metastasis

  , metastasisEven
  , metastasisOdd

  ) where

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.SVGColours ( black, grey )

import Data.AffineSpace                         -- package: vector-space

import Data.List


type DPathF = DPoint2 -> DPath

data Props = Props 
      { line_width      :: Double 
      , fill_colour     :: DRGB
      , stroke_colour   :: DRGB
      }

defaultProps :: Props
defaultProps = Props { line_width     = 1.0
                     , fill_colour    = grey
                     , stroke_colour  = black }

strokeProps :: Props -> (DRGB,[StrokeAttr])
strokeProps props = (stroke_colour props, [ LineWidth $ line_width props
                                          , LineCap CapRound ] ) 

fillProps :: Props -> DRGB
fillProps = fill_colour


data Prefix = Init
            | FromTop
            | FromCtr
            | FromBtm
  deriving (Eq,Ord,Show)

-- TikZ-Timing has more options than these 4...

data VAlign = ATop
            | ACtr
            | ABtm
  deriving (Eq,Ord,Show)
 
div16 :: Fractional u => u -> u
div16 = (0.0625 *)

div8 :: Fractional u => u -> u
div8 = (0.125 *)


width :: Num u  => Int -> u -> u
width w hh = hh * fromIntegral w

makePath :: [DVec2] -> DPathF
makePath vs = \pt -> vertexPath $ map (pt .+^) vs


drawDPathF :: Stroke t => t -> DPathF -> DGraphicF
drawDPathF t pf = wrapG . ostroke t . pf


-- straight line - no transition
--
biLine :: VAlign -> Double -> Int -> Double -> DPathF
biLine ATop ins wi hh = makePath [ vec ins hh,    vec (width wi hh) hh    ]
biLine ACtr ins wi hh = makePath [ vec ins 0,     vec (width wi hh) 0     ]
biLine ABtm ins wi hh = makePath [ vec ins (-hh), vec (width wi hh) (-hh) ]

-- threepoint line 
--
triLine :: (VAlign,VAlign) -> (Double,Double) -> Int -> Double -> DPathF
triLine (s,e) (ins1,ins2) wi hh | s==e       = biLine s ins1 wi hh
                                | otherwise  = makePath [v1, v2, v3]
  where
    vpos ATop = hh
    vpos ACtr = 0
    vpos ABtm = (-hh)

    v1        = vec ins1 (vpos s)
    v2        = vec ins2 (vpos e)
    v3        = vec (width wi hh) (vpos e)
    


lineHigh :: Prefix -> Int -> Double -> Props -> DGraphicF
lineHigh pre wi hh props = drawDPathF (strokeProps props) $ fn pre
   where 
     fn FromCtr = triLine (ACtr,ATop) (0,div16 hh) wi hh
     fn FromBtm = triLine (ABtm,ATop) (0,div8  hh) wi hh
     fn _       = biLine  ATop        0            wi hh




lineLow :: Prefix -> Int -> Double -> Props -> DGraphicF
lineLow pre wi hh props = drawDPathF (strokeProps props) $ fn pre
  where
    fn FromCtr  = triLine (ACtr,ABtm) (0,div16 hh) wi hh
    fn FromTop  = triLine (ATop,ABtm) (0,div8  hh) wi hh
    fn _        = biLine  ABtm        0            wi hh



-- High-impedence Z and Undefined X using the same drawing
-- mechanism.
--
-- Note - Init and FromCtr are different...
--
lineMid :: Prefix -> Int -> Double -> Props -> DGraphicF
lineMid pre wi hh props = drawDPathF (strokeProps props) $ fn pre
  where
    fn FromTop  = triLine (ATop,ACtr) (0,div8 hh)  wi hh
    fn FromBtm  = triLine (ABtm,ACtr) (0,div8 hh)  wi hh
    fn FromCtr  = biLine  ACtr        (div8 hh)    wi hh
    fn _        = biLine  ACtr        0            wi hh




metastasis :: Prefix -> Int -> Metastasis -> Double -> Props -> DGraphicF
metastasis _pre wi start hh props =  
    wrapG . ostroke (strokeProps props) . metastasisPath wi start hh

glitch :: Double -> Props -> DGraphicF
glitch hh props = 
    drawDPathF (strokeProps props) $ makePath [ vvec (-hh), vvec hh ]



fillCamferedRect :: (Fill t, Fractional u) => t -> Int -> u -> GraphicF u
fillCamferedRect t wi halfsize = wrapG . fill t . camferedPath wi halfsize

fillRightCamferedRect :: (Fill t, Fractional u) => t -> Int -> u -> GraphicF u
fillRightCamferedRect t wi halfsize = 
    wrapG . fill t . rightCamferedPath wi halfsize

camferedPath :: Fractional u => Int -> u -> Point2 u -> Path u
camferedPath wi hh leftvmid = 
    vertexPath $ leftvmid : sequence [tl, tr , rightvmid, br, bl] leftvmid
  where
    w           = width wi hh 
    cam         = 0.25 * hh 
    tl          = (.+^ vec cam     hh)
    tr          = (.+^ vec w       hh)
    rightvmid   = (.+^ vec (w+cam) 0)
    br          = (.+^ vec w       (-hh))
    bl          = (.+^ vec cam     (-hh))

-- right camfer extended past the width
--
rightCamferedPath :: Fractional u => Int -> u -> Point2 u -> Path u
rightCamferedPath wi hh leftvmid = 
    vertexPath $ sequence [tl, tr , rightvmid, br, bl] leftvmid
  where
    w           = width wi hh
    cam         = 0.25 * hh
    tl          = (.+^ vec 0       hh)
    tr          = (.+^ vec w       hh)
    rightvmid   = (.+^ vec (w+cam) 0)
    br          = (.+^ vec w       (-hh))
    bl          = (.+^ vec 0       (-hh))



data Metastasis = Even | Odd
  deriving (Eq,Ord,Show)



 
metastasisPath :: Fractional u => Int -> Metastasis -> u -> Point2 u -> Path u 
metastasisPath wi start hh leftvmid
    | start == Even = vertexPath $ expand0 $ metastasisEven wi
    | otherwise     = vertexPath $ expand0 $ metastasisOdd wi
  where
    point n h      = leftvmid .+^ vec (uw * fromIntegral n) h
    uh             = 0.5  * hh
    uw             = 0.25 * uh
  
    expand0 (x:xs) = point x 0 : expandU xs
    expand0 _      = []
    
    expandU [x,y]  = [point x uh, point y 0]
    expandU (x:xs) = point x uh : expandD xs
    expandU _      = []

    expandD [x,y]  = [point x (-uh), point y 0]
    expandD (x:xs) = point x (-uh) : expandU xs
    expandD _      = []

  

metastasisEven :: Int -> [Int]
metastasisEven n = 0 : unfoldr phi 1
  where
    nmax = 8*n
    phi i | i < nmax    = Just (i, i+2)
          | i == nmax+1 = Just (nmax, i+2)
          | otherwise   = Nothing

metastasisOdd :: Int -> [Int]
metastasisOdd n = 2 : unfoldr phi 3
  where
    nmax = 8*n
    phi i | i < nmax    = Just (i, i+2)
          | i == nmax+1 = Just (nmax, i+2)
          | otherwise   = Nothing

