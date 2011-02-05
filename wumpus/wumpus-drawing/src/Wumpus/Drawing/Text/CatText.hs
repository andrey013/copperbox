{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Text
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Left-to-right text.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.CatText
  ( 
   
    CatText
  , leftAlign
  , string

  ) where

import Wumpus.Drawing.Chains
import Wumpus.Drawing.Text.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.Utils.JoinList ( JoinList, ViewL(..), viewl )
import qualified Wumpus.Basic.Utils.JoinList as JL

import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

-- Need to know line width (horizontal) and line count (vertical) 
-- to render...
--
-- Can obviously access line count if we avoid avoid operators 
-- for vertical composition operator and delegate it to rendering
-- instead:
--
-- > rightAlign :: [CatText u] -> PosImage u (BoundingBox u)
--

-- A CatPrim returns a drawing function (AdvGraphic) to be used
-- drawing final rendering.
--
type CatPrim u = CF (u, AdvGraphic u)

newtype CatText u = CatText { cat_text :: JoinList (CatPrim u) }


leftAlign :: (Real u, FromPtSize u, Floating u) 
          => [CatText u] -> LocGraphic u 
leftAlign xs = promoteR1 $ \pt -> 
    (centerSpinePoints line_count 0) >>= \pts -> 
    (mapM evalLine xs)               >>= \ys -> 
    centerToBaseline               >>= \down -> 
    let hw = 0.5 * findMaxWidth ys
        dF = moveStart (displaceVec $ vec (-hw) (-down))
        gs = map (\(_,gf) -> ignoreAns $ dF gf) ys
    in (zipchainM emptyLocGraphic gs pts) `at` pt
  where
    line_count    = length xs
    findMaxWidth  = foldr (\(a,_) x -> max a x) 0

evalLine :: Num u => CatText u -> DrawingInfo (u, AdvGraphic u)
evalLine ct = case viewl $ cat_text ct of
    EmptyL -> return (0,  replaceAns (hvec 0) $ emptyLocGraphic)
    af :< rest -> af >>= \a -> go a (viewl rest)
  where
    go acc     EmptyL     = return acc
    go (dx,af) (mf :< ms) = mf >>= \(u,gf) -> 
                            go (dx+u, af `oplus` gf) (viewl ms)


-- | Note this is not a zip if it has an alt... 
--
zipchainM :: OPlus a 
          => LocImage u a -> [LocImage u a] -> LocChain u -> LocImage u a
zipchainM alt []          _  = promoteR1 $ \pt -> alt `at` pt 
zipchainM alt (img1:imgs) fn = promoteR1 $ \pt -> case fn pt of
    []      -> alt `at` pt
    (p1:ps) -> go (img1 `at` p1) imgs ps
  where
    go acc (g:gs) (p:ps)   = let ans = (g `at` p) in go (acc `oplus` ans) gs ps
    go acc _      _        = acc



blank :: Num u => CatText u
blank = catOne $ return (0, replaceAns (hvec 0) $ emptyLocGraphic)


string :: FromPtSize u => String -> CatText u
string = catOne . stringPrim


-- Note - @fill@ combinators cf. @wl-pprint@ (but left and right) 
-- will be very useful.
--
-- Also PosImages can be inlined in text...
--

catOne :: CatPrim u -> CatText u
catOne = CatText . JL.one 


stringPrim :: FromPtSize u => String -> CatPrim u
stringPrim = escapedPrim . escapeString

escapedPrim :: FromPtSize u => EscapedText -> CatPrim u
escapedPrim esc = textVector esc >>= \v -> 
                  return (vector_x v, replaceAns v $ escapedline esc)
