{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.System
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Translate Haskore's Music datatype to Mullein scores for each instrument.
--
--------------------------------------------------------------------------------


module MulleinHaskore.System where


import Mullein.Core
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import Mullein.SpellingMap

import qualified Mullein.Score       as M
import qualified Mullein.ScorePrint  as PP

import qualified Haskore.Basics      as H

import Control.Applicative
import Control.Monad.Reader

import Data.Foldable ( toList )
import Data.List ( foldl' )
import qualified Data.Map as Map
import Data.Ratio
import Data.Sequence hiding ( null )
import qualified Data.Sequence as S

import qualified Text.PrettyPrint.Leijen as PP

type TransM a = Reader Env a

data Env = Env { transp :: Int, spelling :: SpellingMap }

type InstName = String

type SystemP e = Map.Map InstName e 

type OnsetTime = Duration

type InstLine = (InstName, OnsetTime, Seq M.Element)
type Line  = (OnsetTime, Seq M.Element)

-- A system where each instrument has all parallel overlays split 
-- into seperate lines. 
-- Later the lines can be merged into overlays on demand...
type System = SystemP [Line]


instance Applicative (Reader Env) where
  pure  = return
  (<*>) = ap

--------------------------------------------------------------------------------
-- Helpers to build scores 
instMotif :: InstName -> Key -> MetricalSpec -> System -> M.Motif
instMotif name k m sys = M.motif k m ovs where
    ovs = makeOverlays name sys

linearPart :: M.Motif -> M.Part
linearPart m = M.part [M.phrase m]

---------------------------------------------------------------------------------
-- Translate Haskore's Music so it can be used by Mullein


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



default_instrument :: H.IName
default_instrument = "default"

buildSystem :: SpellingMap -> H.Music -> System
buildSystem smap mus = 
    foldr fn Map.empty $ snd $ runReader (untree 0 default_instrument mus) env
  where
    fn :: InstLine -> System -> System
    fn (name,o,se) m = maybe (Map.insert name [(o,se)] m)
                            (\xs -> Map.insert name ((o,se):xs) m)
                            (Map.lookup name m)
    env = Env { transp = 0, spelling = smap }

untree :: Duration -> H.IName -> H.Music -> TransM (Duration, [InstLine])
untree start instr = step start (instr,start,S.empty) []
  where
    step :: Duration -> InstLine -> [InstLine] -> H.Music -> TransM (Duration, [InstLine])
    step t z zs (H.Note p d xs)     = (\note -> (t+ cDur d, z `snoc` note:zs)) 
                                        <$> mkNote p d xs

    step t z zs (H.Rest d)          = pure (t+d', z `snoc` (Rest d'):zs)
                                      where d' = cDur d


    step t z zs (lhs H.:+: rhs)     = do { (t',z':zs') <- step t z zs lhs
                                         ; step t' z' zs' rhs }


    step t z zs (lhs H.:=: rhs)     = do { (_,zs') <- untree t instr rhs
                                         ; step t z (zs++zs') lhs }

    step t z zs (H.Tempo _ mus)     = step t z zs mus

    step t z zs (H.Trans i mus)     = do { j <- asks transp
                                         ; local (\e -> e {transp=j+i}) 
                                                 (step t z zs mus) }

    step t z zs (H.Instr nom mus)   = (\(_,zs') -> (t,z `add` (zs++zs')))
                                        <$> untree t nom mus

    step t z zs (H.Player _ mus)    = step t z zs mus
    
    step t z zs (H.Phrase _ mus)    = step t z zs mus

    snoc (name,t,se) e = (name,t,se |> e)
    
    add (d,nm,se) xs  | S.null se = xs
                      | otherwise = (d,nm,se):xs


mkNote :: H.Pitch -> H.Dur -> [H.NoteAttribute] -> TransM M.Element
mkNote p d _xs = (\t smap -> Note (M.ScNote (fn p t smap) []) (cDur d))
                   <$> asks transp <*> asks spelling
  where 
    fn pch i smap = rename smap (transpose i (cPitch pch))


-- Haskore - middle_c is (C,5)
-- Mullein - middle_c is c5
                        
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
--------------------------------------------------------------------------------
-- pretty print

printSystem :: Key -> MetricalSpec -> System -> IO ()
printSystem k m sys = 
    mapM_ (\name -> printSystem1 name k m sys) (Map.keys sys) 

printSystem1 :: InstName -> Key -> MetricalSpec -> System -> IO ()
printSystem1 name k m sys = do 
    putStrLn $ "INSTRUMENT: " ++ name
    putStrLn $ PP.displayS sdoc [] 
  where
    sdoc   = PP.renderPretty 0.8 80 (PP.part part)
    part   = linearPart motif
    motif  = instMotif name k m sys
