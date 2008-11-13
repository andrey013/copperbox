--------------------------------------------------------------------------------
-- |
-- Module      :  TranslateMusic
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  Flexible instances, mptc.
--
-- An interface for Haskore to HNotate 
--
--------------------------------------------------------------------------------


module TranslateMusic where

import qualified HNotate as Hn 
import Haskore

import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence

shape :: Music a -> Music ()
shape (Primitive a)    = Primitive (Note 0 ())
shape (m1 :+: m2)      = shape m1 :+: shape m2
shape (m1 :=: m2)      = shape m1 :=: shape m2
shape (Modify ctrl m)  = Modify ctrl (shape m)




 

  
-- instruments :: Music a -> [(Maybe InstrumentName,Music a)]
instruments t = seal $ trav t 0 empty where
  trav (Primitive a)             d se = 
                    (Just (Primitive a), d + duration a, se)
                    
  trav (m1 :+: m2)               d se = 
                    let (m1',d',se')    = trav m1 d se
                        (m2',d'',se'')  = trav m1 d' se'
                    in (m1' `mseq` m2', d, se'')
                    
  trav (m1 :=: m2)               d se = 
                    let (m1',d',se')  = trav m1 d se
                        (m2',d'',se'') = trav m1 d se'
                    in (m1' `mpar` m2', d, se'')
                    
  trav (Modify (Instrument n) m) d se = 
                    let (m',d',se') = trav m d se
                    in (Nothing, d, se |> (Just n, d, m'))

  trav (Modify ctrl m)           d se = 
      let (m',d',se') = trav m d se in 
      maybe (Nothing,d,se') (\a -> (Just $ Modify ctrl a, d,se')) m'
                                      
  seal (Nothing,d,se) = se
  seal (Just m,d,se)  = (Nothing,d,Just m) <| se 

duration :: Primitive a -> Dur 
duration (Note d _)   = d
duration (Rest d)     = d 


mseq :: Maybe (Music a) -> Maybe (Music a) -> Maybe (Music a)
Nothing `mseq` a        = a
a       `mseq` Nothing  = a
Just a  `mseq` Just b   = Just (a :+: b)  
  
mpar :: Maybe (Music a) -> Maybe (Music a) -> Maybe (Music a)
Nothing `mpar` a        = a
a       `mpar` Nothing  = a
Just a  `mpar` Just b   = Just (a :=: b)                                       
    

instrumentCount :: Music a -> Int
instrumentCount (Primitive a)    = 0
instrumentCount (m1 :+: m2)      = instrumentCount m1 + instrumentCount m2
instrumentCount (m1 :=: m2)      = instrumentCount m1 + instrumentCount m2
instrumentCount (Modify ctrl m)  = f ctrl + instrumentCount m
  where
    f (Instrument i)  = 1 
    f _               = 0


-- Unfortunately ctrl is opaque to the fold,
-- so we can't do instrumentCount as a fold
instance F.Foldable Music where
  foldMap f (Primitive a)       = F.foldMap f a
  foldMap f (m1 :+: m2)         = F.foldMap f m1 `mappend` F.foldMap f m2
  foldMap f (m1 :=: m2)         = F.foldMap f m1 `mappend` F.foldMap f m2
  foldMap f (Modify ctrl m)     = F.foldMap f m


instance F.Foldable Primitive where
    foldMap f  (Note dur a)   = f a       
    foldMap f  (Rest dur)     = mempty    

translateMusic :: Music Pitch -> ()
translateMusic (Primitive a)    = ()
translateMusic (m1 :+: m2)      = ()              -- sequential composition
translateMusic (m1 :=: m2)      = ()              -- parallel composition
translateMusic (Modify ctrl m)  = ()         -- modifier

translatePrimitive :: Primitive Pitch -> ()
translatePrimitive (Note dur a)   = ()       
translatePrimitive (Rest dur)     = ()  




translatePitchClass :: PitchClass -> Hn.PitchLabel
translatePitchClass Cf        = flatNote    Hn.C
translatePitchClass C         = naturalNote Hn.C
translatePitchClass Cs        = sharpNote   Hn.C
translatePitchClass Df        = flatNote    Hn.D
translatePitchClass D         = naturalNote Hn.D
translatePitchClass Ds        = sharpNote   Hn.D
translatePitchClass Ef        = flatNote    Hn.E
translatePitchClass E         = naturalNote Hn.E
translatePitchClass Es        = sharpNote   Hn.E
translatePitchClass Ff        = flatNote    Hn.F
translatePitchClass F         = naturalNote Hn.F
translatePitchClass Fs        = sharpNote   Hn.F
translatePitchClass Gf        = flatNote    Hn.G
translatePitchClass G         = naturalNote Hn.G
translatePitchClass Gs        = sharpNote   Hn.G
translatePitchClass Af        = flatNote    Hn.A
translatePitchClass A         = naturalNote Hn.A
translatePitchClass As        = sharpNote   Hn.A
translatePitchClass Bf        = flatNote    Hn.B
translatePitchClass B         = naturalNote Hn.B
translatePitchClass Bs        = sharpNote   Hn.B

flatNote, naturalNote, sharpNote :: Hn.PitchLetter -> Hn.PitchLabel
flatNote l = Hn.PitchLabel l Hn.Flat

naturalNote l = Hn.PitchLabel l Hn.Nat

sharpNote l = Hn.PitchLabel l Hn.Sharp
