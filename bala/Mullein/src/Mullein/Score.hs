{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Score
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Named musical elements (e.g. notes, keys) within the NoteCtx monad
--
--------------------------------------------------------------------------------

module Mullein.Score (
    ( & ),
    evaluatePart,

    part,

    repeated, fsrepeat,
    
    motif,
    primary, addOverlay,
    notelist,

    rest, space, note,
    (%%),    
    
    
 ) where

import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import Mullein.RS

import Control.Applicative
import Data.Ratio



(&) :: Monad m => m a -> m b -> m a
(&) f upd  = upd >> f 


--------------------------------------------------------------------------------
-- build the score


evaluatePart :: Key -> MetricalSpec -> NoteCtx (PartP e) -> PartP e
evaluatePart key mspec mpart = evalRS mpart  st0 env0 where
    st0  = St { prev_note_length = 1%4,
                metrical_spec    = mspec,
                current_key      = key }
    env0 = Env



part :: [NoteCtx (PhraseP e)] -> NoteCtx (PartP e)
part ms = Part <$> sequence ms

repeated :: NoteCtx (MotifP e) -> NoteCtx (PhraseP e)
repeated ma = Repeated <$> ma

fsrepeat :: NoteCtx (MotifP e) 
         -> NoteCtx (MotifP e) 
         -> NoteCtx (MotifP e)
         -> NoteCtx (PhraseP e)
fsrepeat ma mx my = FSRepeat <$> ma <*> mx <*> my 

motif :: NoteCtx (OverlayList e) -> NoteCtx (MotifP e)
motif ovs = bracket 
    <$> gets current_key <*> gets metrical_spec <*> ovs


primary :: NoteCtx [ElementP e] -> NoteCtx (OverlayList e)
primary ms = (\xs -> (xs,[])) <$> ms

addOverlay :: BarNum -> NoteCtx [ElementP e] -> NoteCtx (OverlayList e)
           -> NoteCtx (OverlayList e)
addOverlay n ns os = (\xs (p,xss) -> (p,(n,xs):xss)) <$> ns <*> os


notelist :: [e] -> NoteCtx [e]
notelist ss = return ss 

--


rest :: Duration -> ElementP e
rest = Rest

space :: Duration -> ElementP e
space = Spacer


note :: Pitch -> Duration -> Element
note p d   = Note p d

-- alternative to @note@ with more general type
(%%) :: e -> Duration -> ElementP e
(%%) p d = Note p d



