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
  , char
  , kern
  , newline
  , bracketFontFace

  ) where


import Wumpus.Basic.Graphic
import Wumpus.Basic.Utils.Combinators

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

-- Need a note in wumpus-core and here about space:preserve


-- Graphic or GraphicF ? ans. GraphicF

-- Note - should be compatible with free-label and shape labels.
-- This seems to favour GraphicF.

-- Not quite the same as a trace monad because it needs the 
-- result to a function \"from Point -> ... \"


data Idx = Idx { idx_x :: !Int, idx_y :: !Int }
  deriving (Eq,Ord,Show)


rightn      :: Int -> Idx -> Idx 
rightn n    = star (\s i -> s { idx_x = i+n }) idx_x

down1       :: Idx -> Idx 
down1       = star (\s i -> s { idx_y = i-1, idx_x =0 }) idx_y

-- can track the /user vectors so far/ in the state...


data St u = St 
      { xy_pos          :: Idx
      , font_desc       :: FontAttr
      , horizontal_disp :: PtSize
      , acc_graphic     :: TGraphicF u
      }

-- Vertical distance between baselines.
type VDist = PtSize       

type Env = (VDist, RGBi)

newtype TextM u a = TextM { getTextM :: Env -> St u -> (a, St u) }

-- wrap GraphicF as it has /special/ construction.
--
newtype TGraphicF u = TGraphicF { getTGraphicF :: GraphicF u }

consT :: Num u => (Vec2 u, GraphicF u) -> TGraphicF u -> TGraphicF u
consT (V2 x y, f) tg = TGraphicF $ (f . disp x y) `cc` (getTGraphicF tg) 


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
                              
-- Note - post has to displace in the vertical to get the bottom 
-- line at the base line...

runTextM :: (Num u, FromPtSize u) 
         => PtSize -> (RGBi,FontAttr) -> (TextM u a) -> (a,GraphicF u)
runTextM vdistance (rgb,font) ma = post $ getTextM ma env st
  where
    post (a,s)  = let gf = getTGraphicF $ acc_graphic s
                      h  = fromIntegral $ idx_y $ xy_pos s
                  in (a, gf . vdisp (negate $ h * fromPtSize vdistance))

    env         = (vdistance,rgb) 

    st          = St { xy_pos          = Idx 0 0 
                     , font_desc       = font
                     , horizontal_disp = 0
                     , acc_graphic     = TGraphicF (const emptyG) }


--------------------------------------------------------------------------------
-- State monad ops.
-- Note - not all the state should be accessible. 

setFontAttr :: FontAttr -> TextM u ()
setFontAttr fa = TextM $ \_ s -> ((), s { font_desc = fa })

setsFontAttr :: (FontAttr -> FontAttr) -> TextM u ()
setsFontAttr fn = TextM $ \_ st -> ((), upd st) 
  where
    upd = star (\s i -> s { font_desc = fn i} ) font_desc

getFontAttr :: TextM u FontAttr
getFontAttr = TextM $ \_ s -> (font_desc s,s)   


--------------------------------------------------------------------------------

makeDisplacement :: (Num u, FromPtSize u) 
                 => FontSize -> PtSize -> PtSize -> Idx -> (Vec2 u)
makeDisplacement font_sz lefth vdist (Idx x y) = 
    vec (txt_width + fromPtSize lefth)   
        (fromPtSize vdist * fromIntegral y)
  where
    txt_width = fromPtSize $ textWidth font_sz x


text :: (Num u, FromPtSize u) => String -> TextM u ()
text str = TextM $ \r s -> ((), upd r s)
  where
    upd (vdist,rgb) s@(St idx font h acc) = 
        let g1  = textline (rgb,font) str 
            v   = makeDisplacement (font_size font) h vdist idx
        in s { xy_pos      = rightn (length str) idx
             , acc_graphic = (v,g1) `consT` acc      }


char :: (Num u, FromPtSize u) => Char -> TextM u ()
char ch = TextM $ \r s -> ((), upd r s)
  where
    upd (vdist,rgb) s@(St idx font h acc) = 
        let g1  = textline (rgb,font) [ch] 
            v   = makeDisplacement (font_size font) h vdist idx
        in s { xy_pos      = rightn 1 idx
             , acc_graphic = (v,g1) `consT` acc }


kern :: (Num u, FromPtSize u) => PtSize ->  TextM u ()
kern h = TextM $ \_ s -> ((), upd s)
  where
    upd = star (\s i -> s { horizontal_disp = i + h }) horizontal_disp




newline :: TextM u ()
newline = TextM $ \_ s -> ((), upd s)
  where
    upd = star (\s idx -> s { xy_pos = down1 idx, horizontal_disp = 0})
               xy_pos



bracketFontFace :: FontFace -> TextM u a -> TextM u a
bracketFontFace face mf = do
    old <- getFontAttr
    setsFontAttr fn
    ans <- mf 
    setFontAttr old 
    return ans
  where
    fn attr = attr { font_face = face }