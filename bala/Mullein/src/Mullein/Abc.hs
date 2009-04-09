{-# OPTIONS -Wall #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Abc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Abc
--
--------------------------------------------------------------------------------

module Mullein.Abc where

import Mullein.Core
import qualified Mullein.CoreTypes as C
import Mullein.Duration
import Mullein.Pitch

import Data.Ratio

type Multiplier = Rational

-- The Element datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 
data Element = 
      Note 
        { note_pitch          :: Pitch
        , unl_multiplier      :: Multiplier
        }                  
    | Rest  
        { unl_multiplier      :: Multiplier }
    | Spacer  
        { unl_multiplier      :: Multiplier}
    | Chord 
        { chord_elements      :: [Pitch] 
        , unl_multiplier      :: Multiplier
        }          
    | GraceNotes 
        { grace_elements      :: [GraceNote] }                              
    | Nplet 
        { nplet_multipier     :: Int
        , nplet_elements      :: [Pitch] 
        }                   
  deriving (Show) 

type GraceNote = Pitch

unitRescale :: Duration -> Duration -> Duration
unitRescale unl drn = (dn%dd) / (un%ud) where
    (dn,dd)  = ratioElements drn
    (un,ud)  = ratioElements unl

abcSection :: C.LabelSet -> Duration -> C.Section C.Element -> C.Section Element
abcSection ls u (C.Section xs) = C.Section (fmap (fmap (fmap (abcElement ls u))) xs) 

    
abcElement :: C.LabelSet -> Duration -> C.Element -> Element
abcElement ls u (C.Note p d)      = Note (naturalize ls p) (unitRescale u d)
abcElement _  u (C.Rest d)        = Rest (unitRescale u d) 
abcElement _  u (C.Spacer d)      = Spacer (unitRescale u d) 
abcElement ls u (C.Chord ps d)    = Chord (map (naturalize ls) ps) (unitRescale u d)        
abcElement ls _ (C.GraceNotes ns) = GraceNotes (map (naturalize ls . fst) ns)                          
abcElement ls _ (C.Nplet m _ ps)  = Nplet m (map (naturalize ls) ps)
          