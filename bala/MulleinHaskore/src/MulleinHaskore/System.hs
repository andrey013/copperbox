{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.System
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  Flexible instances, mptc.
--
-- An interface for Haskore to Mullein
--
--------------------------------------------------------------------------------


module MulleinHaskore.System where

-- import MulleinHaskore.MusicFold

import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import qualified Mullein.Score       as M

import qualified Haskore.Basics      as H

import Data.Foldable ( toList )
import Data.List ( foldl' )
import qualified Data.Map as Map
import Data.Ratio
import Data.Sequence hiding ( null )
import qualified Data.Sequence as S

type InstName = String

type SystemP e = Map.Map InstName e 

type OnsetTime = Duration

type InstLine = (InstName, OnsetTime, Seq M.Element)
type Line  = (OnsetTime, Seq M.Element)

-- A system where each instrument has all parallel overlays split 
-- into seperate lines. 
-- The lines can be merged into overlays later...
type System = SystemP [Line]


makeOverlays :: InstName -> System -> OverlayList M.ScNote
makeOverlays name sys = maybe failure mergeParallels $ Map.lookup name sys
  where
    failure = error $ "makeOverlays - could not find instrument " ++ name ++
                      "in the score."  

mergeParallels :: [(Duration,Seq M.Element)] -> OverlayList M.ScNote
mergeParallels []     = ([],[])
mergeParallels (x:xs) = foldl' fn (M.primary $ mkLine x) xs
  where
    fn :: OverlayList M.ScNote -> (Duration, Seq M.Element) -> OverlayList M.ScNote
    fn acc z      = M.addOverlay 0 (mkLine z) acc 

    mkLine :: (Duration, Seq M.Element) -> [M.Element]
    mkLine (0,se) = toList se
    mkLine (d,se) = M.space d : toList se



-- 

default_instrument :: H.IName
default_instrument = "default"

buildSystem :: H.Music -> System
buildSystem = parSystem default_instrument




parSystem :: InstName -> H.Music -> System
parSystem instr = foldr fn Map.empty . snd . untree 0 instr 
  where
   fn :: InstLine -> System -> System
   fn (name,o,se) m = maybe (Map.insert name [(o,se)] m)
                            (\xs -> Map.insert name ((o,se):xs) m)
                            (Map.lookup name m)


untree :: Duration -> H.IName -> H.Music -> (Duration, [InstLine])
untree start instr = step start (instr,start,empty) []
  where
    step :: Duration -> InstLine -> [InstLine] -> H.Music -> (Duration, [InstLine])
    step t z zs (H.Note p d xs)     = (t+d', z `snoc` (Note sc d'):zs) 
                                      where sc  = M.ScNote p' xs'
                                            p'  = cPitch p
                                            d'  = cDur d
                                            xs' = []

    step t z zs (H.Rest d)          = (t+d', z `snoc` (Rest d'):zs)
                                      where d' = cDur d


    step t z zs (lhs H.:+: rhs)     = step t' z' zs' rhs
                                      where (t',z':zs') = step t z zs lhs
    
    step t z zs (lhs H.:=: rhs)     = step t z (zs++zs') lhs
                                      where (_,zs') = untree t instr rhs

    step t z zs (H.Tempo _ mus)     = step t z zs mus

    step t z zs (H.Trans _ mus)     = step t z zs mus

    step t z zs (H.Instr nom mus)   = (t,z `add` (zs++zs')) where 
                                        zs' = snd $ untree t nom mus

                                        add (d,nm,se) xs 
                                            | S.null se = xs
                                            | otherwise = (d,nm,se):xs

    step t z zs (H.Player _ mus)    = step t z zs mus
    
    step t z zs (H.Phrase _ mus)    = step t z zs mus

    snoc (name,t,se) e = (name,t,se |> e)




                        
cPitch :: H.Pitch -> Pitch
cPitch (H.Cf, o)    = Pitch C Flat o
cPitch (H.C,  o)    = Pitch C Nat o
cPitch (H.Cs, o)    = Pitch C Sharp o
cPitch (H.Df, o)    = Pitch D Flat o
cPitch (H.D,  o)    = Pitch D Nat o
cPitch (H.Ds, o)    = Pitch D Sharp o
cPitch (H.Ef, o)    = Pitch E Flat o
cPitch (H.E,  o)    = Pitch E Nat o
cPitch (H.Es, o)    = Pitch E Sharp o
cPitch (H.Ff, o)    = Pitch F Flat o
cPitch (H.F,  o)    = Pitch F Nat o
cPitch (H.Fs, o)    = Pitch F Sharp o
cPitch (H.Gf, o)    = Pitch G Flat o
cPitch (H.G,  o)    = Pitch G Nat o
cPitch (H.Gs, o)    = Pitch G Sharp o
cPitch (H.Af, o)    = Pitch A Flat o
cPitch (H.A,  o)    = Pitch A Nat o
cPitch (H.As, o)    = Pitch A Sharp o
cPitch (H.Bf, o)    = Pitch B Flat o
cPitch (H.B,  o)    = Pitch B Nat o
cPitch (H.Bs, o)    = Pitch B Sharp o


cDur :: H.Dur -> Duration
cDur r = n%d where 
    n = fromIntegral $ numerator r
    d = fromIntegral $ denominator r 

