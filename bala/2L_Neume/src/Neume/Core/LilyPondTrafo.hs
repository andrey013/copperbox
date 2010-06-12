{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondTrafo
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Relative / absolute pitch transformation and relative duration
-- transformations.
--
--------------------------------------------------------------------------------


module Neume.Core.LilyPondTrafo
  (
  ) where

import Neume.Core.ModularSyntax
import Neume.Core.Pitch

import MonadLib.Monads
import Control.Applicative

-- pitch transforms don't change type...

type PitchTrafoLy a = State Pitch a

class LilyPondRelPitch repr where
   trafoLyRelPitch :: HasPitch gly => repr gly -> PitchTrafoLy (repr gly)

mapFst :: (a -> x) -> (a,b) -> (x,b)
mapFst f (a,b) = (f a,b) 




relpMetricalDiv :: HasPitch gly 
                => MetricalDiv gly -> PitchTrafoLy (MetricalDiv gly)
relpMetricalDiv (WrapMD (Atom e))       = atom <$> relPitch e
relpMetricalDiv (WrapMD (N_Plet mp xs)) = n_plet mp <$> mapM relpMetricalDiv xs 
relpMetricalDiv (WrapMD (Beamed    xs)) = beamed    <$> mapM relpMetricalDiv xs


relPitch :: HasPitch gly => gly -> PitchTrafoLy gly
relPitch gly = (\p -> setPitch p gly) <$> relativePitch (getPitch gly)
  

-- | Need to return the \original\ pitch as the state, not the
-- octave modified new value.
--
relativePitch :: Pitch -> PitchTrafoLy Pitch
relativePitch p = get     >>= \ prev ->
                  set p   >>
                  return (setOctave (lyOctaveDist prev p) p)
