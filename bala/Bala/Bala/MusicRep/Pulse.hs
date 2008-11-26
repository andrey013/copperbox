
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


import Bala.Base.Duration
import Bala.Base.Metrical
import Bala.Base.Structural


import Data.Sequence




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

-- to do - coalesce rests probably by supplying a function to do it 
claveMotif :: (Duration -> Event) -> Duration -> ClavePattern -> Motif
claveMotif f d = Motif . fmap fn . getClavePattern where
    fn ClaveOn  = f d
    fn ClaveOff = rest d


     









