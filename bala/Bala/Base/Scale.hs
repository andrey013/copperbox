

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
import Bala.Base.PitchOps
import Bala.Base.BaseExtra

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
  deriving (Eq)


newtype IntervalStructure = IntervalStructure { unIS :: [Int] }



-- Interval patterns
mkIS :: String -> IntervalStructure
mkIS = read



octaveComplete (IntervalStructure xs) = 12 == foldr (+) 0 xs



makeScale :: Pitch -> IntervalStructure -> Scale
makeScale p (IntervalStructure xs) = Scale p $ scanl addSemi p xs


 
--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------



instance Read IntervalStructure where 
  readsPrec _ s = readsParsec intervalStructure s

intervalStructure = IntervalStructure <$> many step
  where step  = choice [whole,half,augsecond]
        whole = 2 <$ char 'W'
        half  = 1 <$ char 'H'
        augsecond = 3 <$ string "A2"
        
          

 
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
instance Show Scale where
  showsPrec _ (Scale r ps) = shows ps

    
      
instance Show IntervalStructure where
  showsPrec _ (IntervalStructure xs) = hsepS $ map step xs
    where
      step 1 = showChar 'H'
      step 2 = showChar 'W'
      step 3 = showString "A2"  
                   