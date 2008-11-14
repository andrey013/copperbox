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

import Bala.Base.Duration
import Bala.Base.Pitch

import HNotate.Fits

import Control.Applicative ( (<$>) )
import Data.Foldable
import Data.Sequence
import Data.Traversable
import Prelude hiding (length, null) 


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

note :: Pitch -> Duration -> Elt
note p d = DEvt (Note p) d 

rest :: Duration -> Elt
rest d = DEvt Rest d

chord :: [Pitch] -> Duration -> Elt
chord ps d = Chord (fromList ps) d

notelist :: NoteListF a
notelist = NoteList empty

bar :: BarF a
bar = Bar empty

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
  
-- if we had resizeLeft & resizeRight fits would generalize to sequences...

{-
class Fits a b => Split a b where
  split :: a -> b -> (a,a)


instance Split (EltF p Duration) Duration where
  split a d = let td = measure a    
-}

           