{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Structural
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- /Structural grouping of elements/
--
--------------------------------------------------------------------------------

module Bala.Base.Structural where

import Bala.Base.BaseExtra (stranspose)
import Bala.Base.Duration
import Bala.Base.Pitch

import HNotate (root, System, system1, (|#) )
import qualified HNotate as H
import HNotate.Fits

import Control.Applicative ( (<$>) )
import Data.Foldable
import Data.Sequence
import Data.Traversable
import Prelude hiding (length, null, foldl, foldr, maximum) 


newtype NoteListF a = NoteList { getNoteList :: Seq (BarF a) }
  deriving (Show)
  
instance Functor NoteListF where
  fmap f (NoteList se)     = NoteList (fmap (fmap f) se)
  
instance Foldable NoteListF where
  foldMap f (NoteList se)     = foldMap (foldMap f) se

instance Traversable NoteListF where
  traverse f (NoteList se)  = NoteList <$> traverse (traverse f) se
      
data BarF a = Bar (Seq a) | Overlay (Seq (Seq a))   
  deriving (Show)
  
instance Functor BarF where
  fmap f (Bar se)       = Bar (fmap f se)
  fmap f (Overlay sse)  = Overlay (fmap (fmap f) sse) 

instance Foldable BarF where
  foldMap f (Bar se)          = foldMap f se
  foldMap f (Overlay sse)     = foldMap (foldMap f) sse

instance Traversable BarF where
  traverse f (Bar se)          = Bar <$> traverse f se
  traverse f (Overlay sse)     = Overlay <$> traverse (traverse f) sse
    

data EltF p d = DEvt (Evt p) d      -- some 'event' that has a duration 
              | Mark Mark            -- a mark that has no duration
              | Chord (Seq p) d
              | AGrace (Seq (p,d)) p d 
              | UGrace p d (Seq (p,d))
  deriving (Eq,Show)
  
data Evt p = Note p | Rest | Spacer
  deriving (Eq,Show)
   
data Mark = Tie      -- | ... more ?       
  deriving (Eq,Show)


class Transpose a where transpose :: (Pitch -> Pitch) -> a -> a

instance Transpose a => Transpose (NoteListF a) where
  transpose f = fmap (transpose f) 
  
instance Transpose a => Transpose (BarF a) where
  transpose f = fmap (transpose f) 

instance Transpose (EltF Pitch d) where
  transpose f (DEvt e d)         = DEvt (transpose f e) d 
  transpose f (Mark z)           = Mark z
  transpose f (Chord se d)       = Chord se d -- can't do anything (?)
  transpose f (AGrace se p d)    = AGrace se (f p) d
  transpose f (UGrace p d se)    = UGrace (f p) d se


instance Transpose (Evt Pitch) where 
  transpose f (Note p) = Note (f p)
  transpose f Rest     = Rest 
  transpose f Spacer   = Spacer

barcount :: NoteListF a -> Int
barcount (NoteList se) = length se
  
-- no Functor, Foldable, Traversable for EltF
type Elt = EltF Pitch Duration

type Bar = BarF (EltF Pitch Duration)

type NoteList = NoteListF (EltF Pitch Duration)

infixl 6 ->-
(->-) :: NoteListF elt -> BarF elt -> NoteListF elt
(->-) (NoteList se) bar = NoteList (se |> bar)

infixl 7 +-
(+-) :: BarF elt -> elt -> BarF elt
(+-) (Bar se) elt = Bar  (se |> elt)


infixr 7 -\-
(-\-) :: BarF elt -> BarF elt -> BarF elt 
(-\-) (Bar sa)      (Bar sb)      = Overlay (singleton sa |> sb)
(-\-) (Overlay ssa) (Bar sb)      = Overlay (ssa |> sb)
(-\-) (Bar sa)      (Overlay ssb) = Overlay (sa <|  ssb)
(-\-) (Overlay ssa) (Overlay ssb) = Overlay (ssa >< ssb)


overlay :: Seq (BarF elt) -> BarF elt
overlay = step . viewl
  where 
    step EmptyL     = error $ "overlay: empty sequence"
    step (a :< se)  = foldl (-\-) a se

note :: Pitch -> Duration -> Elt
note p d = DEvt (Note p) d 

rest :: Duration -> Elt
rest d = DEvt Rest d

chord :: [Pitch] -> Duration -> Elt
chord ps d = Chord (fromList ps) d

tie :: Elt 
tie = Mark Tie

notelist :: NoteListF a
notelist = NoteList empty

bar :: BarF a
bar = Bar empty


remeter :: Duration -> Duration -> NoteList -> NoteList
remeter bar_len asis (NoteList se) = step empty asis (viewl se)
  where
    step acc _    EmptyL      = NoteList $ acc
    step acc disp (e :< se)   = let sbars = remeterBar bar_len disp e
                                    disp' = lastBarDisp (viewr sbars) 
                                in step (acc >< sbars) disp' (viewl se)
    
    
    lastBarDisp :: ViewR Bar -> Duration
    lastBarDisp EmptyR                = duration_zero
    lastBarDisp (_ :> (Bar se))       = sumMeasure se
    lastBarDisp (_ :> (Overlay sse))  = maximum $ 
          foldr (\a -> (:) (sumMeasure a)) [] se
    
    
remeterBar :: Duration -> Duration -> Bar -> Seq Bar
remeterBar bar_len asis (Bar se) = 
  fmap Bar $ remeterSe bar_len asis se


--- transpose....

remeterBar bar_len asis (Overlay sse) = 
    step empty $ viewl $ stranspose $ fmap (remeterSe bar_len asis) sse
  where
    step acc EmptyL      = acc
    step acc (e :< sse)  = let ovs = overlay (fmap Bar e) 
                           in step (acc |> ovs) (viewl sse) 
  


remeterSe :: Duration -> Duration -> Seq Elt -> Seq (Seq Elt)
remeterSe bar_len asis se = asectionHy (|> tie) se asis bar_len 


mkSystem :: String -> NoteList -> System
mkSystem name (NoteList se) = system1 name $ step root (viewl se)
  where
    step acc EmptyL               = acc
    step acc (Overlay ovs :< se)  = let xs = toList (fmap overlay1 ovs) 
                                    in step (acc |# xs) (viewl se) -- doesn't work
                                    -- |# <overlay> doesn't move the current 'position' rightwards
                                    -- it just adds more and more polys at the same place
    step acc (Bar evs :< se)      = step (foldl' addr acc evs) (viewl se)

    overlay1 = foldl' addr root 
    
    addr t (DEvt evt d)        = t |# (mkEvent evt d)
    addr t (Mark m)            = t |# (mkMark m)
    addr t (Chord se d)        = t |# (H.chord (toList se) d)
    addr t (AGrace se p d)     = t |# H.agraces (toList se) |# H.note p d
    addr t (UGrace p d se)     = t |# H.note p d |# H.ugraces (toList se)
    
    
    mkEvent (Note p)  d = H.note p d
    mkEvent Rest      d = H.rest d
    mkEvent Spacer    d = H.spacer d
    
    mkMark Tie          = H.tie 
    
    
-- aka null
class Null a where isNull :: a -> Bool

instance Null (NoteListF a) where
  isNull (NoteList se) = null se
   
   
instance Null (BarF a) where
  isNull (Bar se)       = null se
  isNull (Overlay sse)  = null sse
  
  


--------------------------------------------------------------------------------
-- fitting

instance Fits (EltF p Duration) Duration where
  measure (DEvt e d)          = d
  measure (Mark z)            = duration_zero
  measure (Chord se d)        = d 
  measure (AGrace se p d)     = d
  measure (UGrace p d se)     = d
  
  resizeTo (DEvt e _)        d = DEvt e d
  resizeTo (Mark z)          d = Mark z
  resizeTo (Chord se _)      d = Chord se d 
  resizeTo (AGrace se p _)   d = AGrace se p d
  resizeTo (UGrace p _ se)   d = UGrace p d se
  

    