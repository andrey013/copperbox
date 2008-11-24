
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.OutputHNotate
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output Midi for Structural2
--
--------------------------------------------------------------------------------



module Bala.Base.OutputHNotate where

import Bala.Base.BaseExtra
import Bala.Base.Duration
import Bala.Base.Pitch
import Bala.Base.Structural

import qualified HNotate as H
import HNotate ( (|#) )

import qualified Data.Foldable as F
import Data.Sequence
import Data.Word

generateEventList :: Section -> H.EventList
generateEventList sn = 
    F.foldl phraseFoldStep H.root $ getPhrases $ packToLength sn
  where
    getPhrases (Section _ se) = se    


phraseFoldStep :: H.EventList -> Phrase -> H.EventList
phraseFoldStep t (Single mo)      = t `addMotif` mo
phraseFoldStep t (Overlay mo smo) = t |# (f mo : ovs (viewl smo)) where
    ovs :: ViewL Motif -> [H.EventList]
    ovs EmptyL     = []
    ovs (a :< sa)  = f a : ovs (viewl sa)
    
    f :: Motif -> H.EventList
    f mo = H.root `addMotif` mo
    

addMotif :: H.EventList -> Motif -> H.EventList
addMotif t (Motif se) = F.foldl motifFoldStep t se


motifFoldStep :: H.EventList -> Event -> H.EventList
motifFoldStep t (Note p d)          = t |# (H.note p d)
motifFoldStep t (Rest d)            = t |# (H.rest d)
motifFoldStep t (Chord se d)        = t |# (H.chord se d)
motifFoldStep t (Spacer d)          = t |# (H.spacer d)
motifFoldStep t (AGrace se p d)     = t |# H.agraces se |# H.note p d
motifFoldStep t (UGrace p d se)     = t |# H.note p d |# H.ugraces se
motifFoldStep t (Mark m)            = t |# (mkMark m)


mkMark Tie          = H.tie 
    




            