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
-- \*\* WARNING \*\* this module is considered obsolete.
-- 
-- LRText no longer seems a satisfactory way to build text.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.LRText
  ( 


    LRText

  , runLRText
  , execLRText

  , kern
  , char
  , escInt
  , escName
  , symb
  , symbEscInt
  , symbEscName

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Utils.HList

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad




-- Need a note in wumpus-core and here about space:preserve

-- Note - if we have font change (e.g. to symbol font) then we 
-- have to generate more than one hkernline.
-- 
-- Apropos /optimization/ we have two two simultaneous and 
-- overlayed lines - one in the regular font, and one in Symbol. 
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

newtype LRText u a = LRText { getLRText :: Env u -> St u -> (a, St u) }

type instance MonUnit (LRText u) = u

instance Functor (LRText u) where
  fmap f mf = LRText $ \r s -> let (a,s') = getLRText mf r s in (f a,s')


instance Applicative (LRText u) where
  pure a    = LRText $ \_ s -> (a,s)
  mf <*> ma = LRText $ \r s -> let (f,s')  = getLRText mf r s
                                   (a,s'') = getLRText ma r s'
                               in (f a,s'')

instance Monad (LRText u) where
  return a  = LRText $ \_ s -> (a,s)
  m >>= k   = LRText $ \r s -> let (a,s')  = getLRText m r s 
                               in (getLRText . k) a r s'




runLRText :: (Num u, FromPtSize u) => LRText u a -> LocImage u a
runLRText ma = promote1 $ \pt -> envZero >>= \e1 -> 
    let (a,st) = getLRText ma e1 st_zero 
    in mkline pt (acc_chr st) >>= \g1 ->
       localize (fontface symbol) (mkline pt (acc_sym st)) >>= \g2 ->
       return (a, g1 `oplus` g2)
  where
    mkline pt h = hkernline (toListH h) `at` pt


execLRText :: (Num u, FromPtSize u) => LRText u a -> LocGraphic u
execLRText ma = postpro1 snd $ runLRText ma

st_zero :: Num u => St u
st_zero = St { delta_chr       = 0
             , delta_sym       = 0
             , acc_chr         = emptyH
             , acc_sym         = emptyH }
 

envZero :: FromPtSize u => CF (Env u)
envZero = (\sz -> Env { char_width   = fromPtSize $ charWidth sz
                      , spacer_width = fromPtSize $ spacerWidth sz })
            <$> fontSize


gets :: (St u -> a) -> LRText u a
gets fn = LRText $ \_ s -> (fn s, s)

charMove :: Num u => LRText u ()
charMove = LRText $ \(Env {char_width=cw, spacer_width=sw}) s -> 
             let step_width = cw + sw    
                 d_sym      = (delta_sym s) + step_width
             in ((), s { delta_chr = step_width, delta_sym = d_sym }) 

symbMove :: Num u => LRText u ()
symbMove = LRText $ \(Env {char_width=cw, spacer_width=sw}) s -> 
             let step_width = cw + sw
                 d_chr      = (delta_chr s) + step_width
             in ((), s { delta_chr = d_chr, delta_sym = step_width }) 

snocSymb :: KerningChar u -> LRText u ()
snocSymb kc = LRText $ \_ s -> ((), upd s)
  where
    upd = (\s a -> s { acc_sym = a `snocH` kc}) <*> acc_sym

snocChar :: KerningChar u -> LRText u ()
snocChar kc = LRText $ \_ s -> ((), upd s)
  where
    upd = (\s a -> s { acc_chr = a `snocH` kc}) <*> acc_chr

kern :: Num u => u -> LRText u ()
kern dx = LRText $ \_ s -> ((), upd s)
  where
    upd = (\s a b -> s { delta_chr = a+dx, delta_sym = b+dx}) 
            <*> delta_chr <*> delta_sym

char :: Num u => Char -> LRText u ()
char ch = gets delta_chr           >>= \u -> 
          snocChar (kernchar u ch) >> charMove

escInt ::  Num u => Int -> LRText u ()
escInt i = gets delta_chr            >>= \u -> 
           snocChar (kernEscInt u i) >> charMove

escName ::  Num u => String -> LRText u ()
escName s = gets delta_chr             >>= \u -> 
            snocChar (kernEscName u s) >> charMove


symb :: Num u => Char -> LRText u ()
symb sy = gets delta_sym           >>= \u -> 
          snocSymb (kernchar u sy) >> symbMove


symbEscInt :: Num u => Int -> LRText u ()
symbEscInt i = gets delta_sym            >>= \u -> 
               snocSymb (kernEscInt u i) >> symbMove

symbEscName :: Num u => String -> LRText u ()
symbEscName s = gets delta_sym             >>= \u -> 
                snocSymb (kernEscName u s) >> symbMove
