

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Scale
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A datatypes for representing pitch
-- |
--------------------------------------------------------------------------------

module Bala.Base.Scale where

import Bala.Base.PitchRep
import Bala.Base.Interval
import Bala.Base.PitchOps
import Bala.Base.BaseExtra
import Bala.Base.AffiDecoInstances

import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 

--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

data Scale = Scale {
    scale_root  :: Pitch,
    scale_notes :: [Pitch]
    }
  
data ScaleDegree = Tonic | SuperTonic | Mediant | Subdominant | Dominant
                 | Submediant | LeadingTone 
  deriving (Eq,Show)

newtype IntervalStructure = IntervalStructure { unIS :: [Interval] }
  deriving (Show)




-- Interval patterns
mkIS :: String -> IntervalStructure
mkIS = decouper



octaveComplete (IntervalStructure xs) = 12 == foldr fn 0 xs
  where fn ival n = n + halfSteps ival



makeScale :: Pitch -> IntervalStructure -> Scale
makeScale p (IntervalStructure xs) = Scale p $ scanl extr p xs

 
--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------


        
instance Deco IntervalStructure where 
  deco = decoIntervalStructure
  
decoIntervalStructure  = IntervalStructure <$> many1 step
  where step = choice [whole,half,augsecond]
        whole = interval 2 2 <$ char 'W'
        half  = interval 2 1 <$ char 'H'
        augsecond = interval 2 3 <$ string "A2"

         
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
instance Affi Scale where
  affi (Scale r ps) = hsepS $ map affi ps

    
      
instance Affi IntervalStructure where
  affi (IntervalStructure xs) = hsepS $ map (step . halfSteps) xs
    where
      step 1 = showChar 'H'
      step 2 = showChar 'W'
      step 3 = showString "A2"  
                   