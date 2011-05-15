{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.Chain
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Chaining moveable LocGraphics.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.Chain
  (
    ChainAlg
  , IterationScheme
  
  , chain
  , chain_
  
  , linearChain
  , prefixChain
  , iterationScheme

  , chainIterate
  , chainH
  , chainV

  , tableRight  
  , tableDown

  , radialChain

  ) where


import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Objects.Basis
import Wumpus.Basic.Kernel.Objects.Displacement
import Wumpus.Basic.Kernel.Objects.LocImage
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid

-- In TikZ chains are finite node list and iterated (infite) points


-- | Chain algorithm.
-- 
-- @Linear@ simply iterates points.
--
-- @Prefix@ runs the left chain @n@ times then runs the right 
-- chain from the end point of the left chain.
-- 
data ChainAlg u = L1 (IterationScheme u)
                | PX Int (ChainAlg u) (ChainAlg u)




type instance DUnit (ChainAlg u) = u



data IterationScheme u = forall st. Scheme 
      { scheme_start    :: Point2 u -> st
      , scheme_step     :: st -> (st, Point2 u)
      }

type instance DUnit (IterationScheme u) = u




-- | Note the tail of of result list is infinite.
-- 
-- As any prefixes will be generated fully. This potentially
-- produces a /space bubble/ where a long prefix has to be 
-- generated without the streaming of lazy evaluation. However,
-- chains that are long enough to cause this problem will be 
-- problematic for a PostScript or SVG renderer.
--
-- In short - don'\t make long chains.
--
interpChainAlg :: ChainAlg u -> Point2 u -> [Point2 u]
interpChainAlg ch start = go start ch
  where
    go pt (L1 (Scheme mk run)) = let st = mk pt in runInf run st
    go pt (PX n chl chr)       = let (af,end) = takeAndLast n (go pt chl)
                                 in prefixListH af $ go end chr


runInf :: (st -> (st,Point2 u)) -> st -> [Point2 u]
runInf fn = \st -> go (fn st) 
  where
    go (st,a) = a : go (fn st)




-- | Take n elements - also return the last of element in the 
-- tuple so it can be accessed without a second traversal.
--
-- Note @(n > 0)@ 
-- 
takeAndLast :: Int -> [a] -> (H a,a)
takeAndLast _ []      = error "takeAndLast - empty list (unreachable?)"
takeAndLast n (a:as)  = go (wrapH a,a) (n-1) as
  where
    go (af,_) i (x:xs) | i > 0     = go (af `snocH` x, x) (i-1) xs
    go acc    _ _                  = acc

emptyLoc :: LocGraphic u
emptyLoc = mempty

-- | Returns the end point...
--
chain :: InterpretUnit u
      => ChainAlg u -> [LocImage u a] -> LocImage u (Point2 u)
chain _   [] = promoteU $ \pt -> replaceAns pt (applyU emptyLoc pt)
chain alg fs = promoteU $ \pt -> 
    let ps = interpChainAlg alg pt in go1 fs pt ps 
  where
    go1 (gf:gs) _  (p:ps)     = go (ignoreAns $ applyU gf p) gs p ps
    go1 _       p0 _          = replaceAns p0 $ applyU emptyLoc p0

    go acc (gf:gs) _  (p:ps)  = let g1 = ignoreAns $ applyU gf p
                                in go (acc `mappend` g1) gs  p ps
    go acc _       p0 _       = replaceAns p0 acc


-- | Returns no answer, just a 'LocGraphic'.
chain_ :: InterpretUnit u => ChainAlg u -> [LocImage u a] -> LocGraphic u
chain_ alg xs = fmap (const UNil) $ chain alg xs


linearChain :: IterationScheme u -> ChainAlg u
linearChain = L1

prefixChain :: Int -> ChainAlg u -> ChainAlg u -> ChainAlg u
prefixChain n c1 c2 | n < 1     = c2
                    | otherwise = PX n c1 c2


iterationScheme :: (Point2 u -> st) 
                -> (st -> (st, Point2 u)) 
                -> IterationScheme u
iterationScheme start stepper = Scheme start stepper




chainIterate :: (Point2 u -> Point2 u) -> ChainAlg u
chainIterate fn = L1 $ Scheme { scheme_start = id
                              , scheme_step  = \pt -> (fn pt, pt)
                              }


chainH :: Num u => u -> ChainAlg u
chainH = L1 . scHorizontal

scHorizontal :: Num u => u -> IterationScheme u
scHorizontal dx = Scheme { scheme_start = id
                         , scheme_step  = \pt -> (displace (hvec dx) pt, pt)
                         }
   


chainV :: Num u => u -> ChainAlg u
chainV = L1 . scVertical

scVertical :: Num u => u -> IterationScheme u
scVertical dy = Scheme { scheme_start = id
                       , scheme_step  = \pt -> (displace (vvec dy) pt, pt)
                       }


tableRight :: Num u => Int -> (u,u) -> ChainAlg u
tableRight num_cols (col_width,row_height) = 
    L1 $ scStepper downF num_cols rightF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width

tableDown :: Num u => Int -> (u,u) -> ChainAlg u
tableDown num_rows (col_width,row_height) = 
    L1 $ scStepper rightF num_rows downF
  where
    downF   = displace $ vvec $ negate row_height
    rightF  = displace $ hvec col_width


-- | Outer and inner steppers.
--
scStepper :: PointDisplace u -> Int -> PointDisplace u -> IterationScheme u
scStepper outF n innF = Scheme start step
  where
    start pt                      = (pt,pt,0)
    step  (ogin,pt,i) | i < n     = ((ogin, innF pt, i+1), pt)
                      | otherwise = let o1 = outF ogin 
                                    in ((o1, innF o1,1), o1) 




radialChain :: Floating u => u -> Radian -> Radian -> ChainAlg u
radialChain radius start step = L1 $ scCircular radius start step

   
scCircular :: Floating u => u -> Radian -> Radian -> IterationScheme u
scCircular radius angstart angseg = Scheme start step 
  where
    start pt        = (pt,angstart)
    step (ogin,ang) = ((ogin,ang + angseg), displace (avec ang radius) ogin)


