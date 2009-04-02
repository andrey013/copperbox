{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.AbcForm
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Building note lists
--
--
--------------------------------------------------------------------------------

module HNotate.AbcForm where

import HNotate.Duration
import HNotate.MusicRepDatatypes
import qualified HNotate.NoteList as N
import HNotate.Pitch
import HNotate.Staff
import HNotate.Utils ( prime )


import Data.Ratio
import Text.PrettyPrint.Leijen

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

abcS :: LabelSet -> Duration -> Staff N.Element -> Staff Element
abcS ls u (Staff xs) = Staff (fmap (fmap (fmap (abcE ls u))) xs) 

    
abcE :: LabelSet -> Duration -> N.Element -> Element
abcE ls u (N.Note p d)      = Note (naturalize ls p) (unitRescale u d)
abcE _  u (N.Rest d)        = Rest (unitRescale u d) 
abcE _  u (N.Spacer d)      = Spacer (unitRescale u d) 
abcE ls u (N.Chord ps d)    = Chord (map (naturalize ls) ps) (unitRescale u d)        
abcE ls _ (N.GraceNotes ns) = GraceNotes (map (naturalize ls . fst) ns)                          
abcE ls _ (N.Nplet m _ ps)  = Nplet m (map (naturalize ls) ps)

--------------------------------------------------------------------------------
-- pretty print

instance Pretty Element where
  pretty (Note p m)           = pretty p <> prime <> modifier m
  pretty (Rest m)             = char 'r' <> modifier m 
  pretty (Spacer m)           = char 's' <> modifier m
  pretty (Chord se m)         = brackets (hsep $ fmap pretty se) 
                                    <> prime <> modifier m
  pretty (GraceNotes se)      = braces (hsep $ fmap pretty se)
  pretty (Nplet _ se)       = braces (hsep $ fmap pretty se)    

modifier :: Multiplier -> Doc
modifier m | m == 1     = empty
           | otherwise  = text $ show m


