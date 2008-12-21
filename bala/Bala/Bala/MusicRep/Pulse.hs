{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.MusicRep.Pulse
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Develop clave patterns
--
--------------------------------------------------------------------------------


module Bala.MusicRep.Pulse where

import qualified Bala.Base.BaseExtra as S
import Bala.Base.Duration
import Bala.Base.Metrical
import Bala.Base.Pitch
import Bala.Base.Structural

import Data.Sequence
import Prelude hiding (null) 



clavel :: [Clave] -> ClavePattern
clavel = ClavePattern . fromList


-- n and k are flipped from bjorklund...    
euclidRhythm :: Int -> Int -> ClavePattern
euclidRhythm k n = bjorklund n k

readClave :: Eq a => a -> [a] -> ClavePattern
readClave a = ClavePattern . fromList . step where 
    step (x:xs) | x == a    = ClaveOn  : step xs
                |otherwise  = ClaveOff : step xs
    step []                 = []


-- make clave patterns as singletons of notes and drumchords...

claveMotif :: Duration -> [(Pitch,ClavePattern)] -> Motif
claveMotif d = 
    Motif . fmap (pgroup . ons) . S.transpose . fromList . fmap (uncurry line) 
  where
    line :: Pitch -> ClavePattern -> Seq (Pitch,Clave)
    line p = fmap (\c -> (p,c)) . getClavePattern
    
    ons :: Seq (Pitch,Clave) -> Seq Pitch
    ons = fmap fst . S.filter (\(_,c) -> c == ClaveOn)

    pgroup :: Seq Pitch -> Event
    pgroup se = case viewl se of
        EmptyL -> rest d
        a :< sa -> if null sa then note a d else ChordE se d
        
        






