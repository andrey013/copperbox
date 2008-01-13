
module Sound.Bala.Base.Triad where

import Sound.Bala.Base.PitchRep

import Sound.Bala.Base.ReadPExtra

import Control.Applicative hiding (many, optional)
import Control.Monad (ap)
import Text.ParserCombinators.ReadP


newtype Triad = Triad {unTriad :: (Int,Int,Int)}

instance Read Triad where 
  readsPrec i s = readP_to_S readTriad s

readTriad :: ReadP Triad  
readTriad = undefined



