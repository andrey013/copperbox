{-# LANGUAGE TypeFamilies               #-}
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
-- LRText monad - left-to-right text, with kerning.
-- 
-- Note - because Wumpus has no access to the metrics data inside
-- a font file, the default spacing is not good and it is 
-- expected that kerning will need to be added per-letter for
-- variable width fonts.
--
-- This module makes precise text spacing \*possible\* - it does 
-- not make it \*easy\*.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.LRText
  ( 


    TextM
  , runTextM
  , execTextM

  , kern
  , char
  , symb
  , symbi
  , symbEscInt

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad
import Data.Char ( chr )
import Data.Monoid





-- Need a note in wumpus-core and here about space:preserve

-- Note - if we have font change (e.g. to symbol font) then we 
-- have to generate more than one hkernline.
-- 
-- If one wants to be really prissy about optimization, one could
-- generate two simultaneous and overlayed lines - one in regular 
-- font, one in Symbol. 
--
-- Result should be a LocGraphic (so cannot do a trace as we go). 
--

-- Note - the the state tracks two kernlines: one for symbols,  

data St u = St 
      { delta_chr       :: !u
      , delta_sym       :: !u
      , acc_chr         :: H (KerningChar u)
      , acc_sym         :: H (KerningChar u)
      }


data Env u = Env 
      { char_width      :: !u
      , spacer_width    :: !u
      }

-- Note - unlike Turtle for example, Text is a monad not a 
-- transformer.
--
-- The rationale for this is to avoid complications percolating 
-- from the Drawing monad. It Text were built over the Drawing
-- monad what would it do on a font change, a colour change...
-- 
-- That say Text must still be run /within/ the Drawing so it 
-- can take the initial font size, stroke colour etc.
--

newtype TextM u a = TextM { getTextM :: Env u -> St u -> (a, St u) }

type instance MonUnit (TextM u) = u

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

runTextM :: (Num u, FromPtSize u, DrawingCtxM m, u ~ MonUnit m) 
         => TextM u a -> m (a, LocGraphic u)
runTextM ma = askCtx >>= \ctx -> 
    let e = runDF ctx envZero in post $ getTextM ma e stZero
  where
    post (a,st)  = let sG = updCtxSym $ mkHKern $ toListH $ acc_sym st
                       cG = mkHKern $ toListH $ acc_chr st
                   in return (a, sG `lgappend` cG)

    mkHKern []   = const mempty
    mkHKern xs   = hkernline xs

    updCtxSym lg = localLG (fontface symbol) lg


execTextM :: (Num u, FromPtSize u, DrawingCtxM m, u ~ MonUnit m) 
          => TextM u a -> m (LocGraphic u)
execTextM ma = liftM snd $ runTextM ma  


stZero :: Num u => St u
stZero = St { delta_chr       = 0
            , delta_sym       = 0
            , acc_chr         = emptyH
            , acc_sym         = emptyH }

envZero :: FromPtSize u => DrawingF (Env u)
envZero = (\sz -> Env { char_width   = fromPtSize $ charWidth sz
                      , spacer_width = fromPtSize $ spacerWidth sz })
            <$> asksDF (font_size . font_props)


gets :: (St u -> a) -> TextM u a
gets fn = TextM $ \_ s -> (fn s, s)

charMove :: Num u => TextM u ()
charMove = TextM $ \(Env {char_width=cw, spacer_width=sw}) s -> 
             let step_width = cw + sw    
                 d_sym      = (delta_sym s) + step_width
             in ((), s { delta_chr = step_width, delta_sym = d_sym }) 

symbMove :: Num u => TextM u ()
symbMove = TextM $ \(Env {char_width=cw, spacer_width=sw}) s -> 
             let step_width = cw + sw
                 d_chr      = (delta_chr s) + step_width
             in ((), s { delta_chr = d_chr, delta_sym = step_width }) 

snocSymb :: KerningChar u -> TextM u ()
snocSymb kc = TextM $ \_ s -> ((), upd s)
  where
    upd = (\s a -> s { acc_sym = a `snocH` kc}) <*> acc_sym

snocChar :: KerningChar u -> TextM u ()
snocChar kc = TextM $ \_ s -> ((), upd s)
  where
    upd = (\s a -> s { acc_chr = a `snocH` kc}) <*> acc_chr

kern :: Num u => u -> TextM u ()
kern dx = TextM $ \_ s -> ((), upd s)
  where
    upd = (\s a b -> s { delta_chr = a+dx, delta_sym = b+dx}) 
            <*> delta_chr <*> delta_sym

char :: Num u => Char -> TextM u ()
char ch = gets delta_chr           >>= \u -> 
          snocChar (kernchar u ch) >> charMove

symb :: Num u => Char -> TextM u ()
symb sy = gets delta_sym           >>= \u -> 
          snocSymb (kernchar u sy) >> symbMove


symbi :: Num u => Int -> TextM u ()
symbi i = symb (chr i) 


symbEscInt :: Num u => Int -> TextM u ()
symbEscInt i = gets delta_sym            >>= \u -> 
               snocSymb (kernEscInt u i) >> symbMove

