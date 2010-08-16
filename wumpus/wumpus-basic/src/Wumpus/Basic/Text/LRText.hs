{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.LRText
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- LRText monad - left-to-right text.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.LRText
  ( 
    TextM
  , runTextM

  , text

  ) where


import Wumpus.Basic.Graphic
import Wumpus.Basic.Utils.Combinators

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative


-- Graphic or GraphicF ? ans. GraphicF

-- Note - should be compatible with free-label and shape labels.
-- This seems to favour GraphicF.

-- Not quite the same as a trace monad because it needs the 
-- result to a function \"from Point -> ... \"


data Idx = Idx { idx_x :: !Int, idx_y :: !Int }
  deriving (Eq,Ord,Show)


rightn      :: Int -> Idx -> Idx 
rightn n    = star (\s i -> s { idx_x = i+n} ) idx_x

down1       :: Idx -> Idx 
down1       = star (\s i -> s { idx_y = i+1} ) idx_y

-- can track the /user vectors so far/ in the state...


data St u = St 
      { xy_pos          :: Idx
      , font_desc       :: FontAttr
      , horizontal_disp :: PtSize
      , acc_graphic     :: TGraphicF u
      }

-- Vertical distance between baselines.
type VDist = PtSize       

type Env = (VDist, DRGB)

newtype TextM u a = TextM { getTextM :: Env -> St u -> (a, St u) }

-- wrap GraphicF as it has /special/ construction.
--
newtype TGraphicF u = TGraphicF { getTGraphicF :: GraphicF u }

snocT :: Num u => TGraphicF u -> (Vec2 u, GraphicF u) -> TGraphicF u
snocT tg (V2 x y, f) = TGraphicF $ (getTGraphicF tg) `cc` (f . disp x y)


instance Functor (TextM u) where
  fmap f mf = TextM $ \r s -> let (a,s') = getTextM mf r s in (f a,s')

instance Applicative (TextM u) where
  pure a    = TextM $ \_ s -> (a,s)
  mf <*> ma = TextM $ \r s -> let (f,s')  = getTextM mf r s
                                  (a,s'') = getTextM ma r s'
                              in (f a,s'')

instance Monad (TextM u) where
  return a  = TextM $ \_ s -> (a,s)
  m >>= k   = TextM $ \r s -> let (a,s')  = getTextM m r s 
                              in (getTextM . k) a r s'
                              

runTextM :: PtSize -> (DRGB,FontAttr) -> (TextM u a) -> (a,GraphicF u)
runTextM vdistance (rgb,font) ma = post $ getTextM ma env st
  where
    post (a,s)  = (a, getTGraphicF $ acc_graphic s)
    env         = (vdistance,rgb) 
    st          = St { xy_pos          = Idx 0 0 
                     , font_desc       = font
                     , horizontal_disp = 0
                     , acc_graphic     = TGraphicF (const emptyG) }


-- TODO tidy up...
text :: (Num u, FromPtSize u) => String -> TextM u ()
text str = TextM $ \r s -> ((), upd r s)
  where
    len       = length str
    upd (vdist,rgb) = star4 (\s idx font h acc -> 
                             let g1 = textline (rgb,font) str 
                                 v  = makeDisplacement (font_size font) h vdist idx
                             in s { xy_pos = rightn len idx
                                  , acc_graphic = acc `snocT` (v,g1) })
                            xy_pos 
                            font_desc
                            horizontal_disp
                            acc_graphic

makeDisplacement :: (Num u, FromPtSize u) 
                 => FontSize -> PtSize -> PtSize -> Idx -> (Vec2 u)
makeDisplacement font_sz lefth vdist (Idx x y) = 
    vec ((ch_width * fromIntegral x) - fromPtSize lefth)   
        (fromPtSize vdist * fromIntegral y)
  where
    ch_width = fromPtSize $ charWidth font_sz

