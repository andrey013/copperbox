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

    part,

    repeated, fsrepeat,
    
    motif,
    primary, addOverlay,
--    notelist,

    rest, space, note,
    (%%),    
    
    
 ) where

import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch




part :: [PhraseP e] -> PartP e
part = Part

repeated :: MotifP e -> PhraseP e
repeated = Repeated

fsrepeat :: MotifP e
         -> MotifP e
         -> MotifP e
         -> PhraseP e
fsrepeat = FSRepeat

motif :: Key -> MetricalSpec -> OverlayList e -> MotifP e
motif = bracket


primary :: [ElementP e] -> OverlayList e
primary xs = (xs,[])

addOverlay :: BarNum -> [ElementP e] -> (OverlayList e)
           -> (OverlayList e)
addOverlay n xs (p,xss) = (p,(n,xs):xss)


-- notelist :: [e] -> NoteCtx [e]
-- notelist ss = return ss 

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



