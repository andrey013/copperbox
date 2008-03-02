

module Korda where

import Bala

import Data.Maybe
import Control.Applicative hiding (many, optional, (<|>) )
import Text.ParserCombinators.Parsec 

data FingerPos = FPos Int | NotF 

  
data Fingering = Fingering (Maybe Int) [FingerPos] 

c_slash_d :: Fingering
c_slash_d = read "(xx0010)"

c_slash_b :: Fingering
c_slash_b = read "(x22010)"


lower_e :: Pitch
lower_e = read "E4"
    
standard_tuning = map (addSemi lower_e) (zac 0 [5,5,5,4,5])

-- evalFingering :: Fingering -> [Int]
evalFingering (Fingering _ xs) = map (addSemi lower_e) $ offsets xs
  where 

    offsets xs    = catMaybes $ zipWith fn xs (zac 0 [5,5,5,4,5])
    fn NotF     _ = Nothing
    fn (FPos i) j = Just (i + j)
        
demo = evalFingering c_slash_d

--------------------------------------------------------------------------------
-- Read instances
--------------------------------------------------------------------------------

instance Read Fingering where 
  readsPrec _ s = readsParsec fingering s

fingering = Fingering <$> optparse fretpos <*> parens (many1 fingerPos)
  where fretpos = int <* char '@'

  
instance Read FingerPos where 
  readsPrec _ s = readsParsec fingerPos s  
  
fingerPos = fingered <|> notfingered
  where fingered    = FPos <$> digiti
        notfingered = NotF <$  char 'x'
          

 
--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------
  
instance Show Fingering where
  showsPrec _ (Fingering fret xs) 
      = prefix fret . (withParens $ showTogether xs)
    where
      prefix Nothing  = id
      prefix (Just i) = showChar '@' . shows i
    
instance Show FingerPos where
  showsPrec _ NotF     = showChar 'x'
  showsPrec _ (FPos i) = shows i


