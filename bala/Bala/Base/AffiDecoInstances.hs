


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.AffiDecoInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Affi & Deco alternatives to Read and Show
-- |
--------------------------------------------------------------------------------

module Bala.Base.AffiDecoInstances where

import Bala.Base.BaseExtra
import Bala.Base.PitchRep
import Bala.Base.PitchOps
import Bala.Base.Interval

import Control.Applicative hiding (many, optional, (<|>) )
import Control.Monad (ap)
import Text.ParserCombinators.Parsec

--------------------------------------------------------------------------------
-- Deco instances
--------------------------------------------------------------------------------


instance Deco Pitch where 
  deco = decoPitch

decoPitch :: Parser Pitch
decoPitch = buildPitch <$> deco
                       <*> option 4 positiveInt 
                       <*> option 0 signedInt
                       
                       
instance Deco PitchLabel where
  deco = decoPitchLabel

decoPitchLabel = PitchLabel <$> decoPitchLetter <*> decoAccidental
-- or decoPitchLabel = PitchLabel <$> deco <*> deco

decoPitchLabel' = deco2 PitchLabel
 
deco2 :: (Deco a, Deco b) => (a -> b -> c) -> Parser c
deco2 fn = fn <$> deco <*> deco 


instance Deco PitchLetter where
  deco = decoPitchLetter
    
decoPitchLetter = letter <$> oneOf "ABCDEFG" 
  where 
    letter 'A' = A
    letter 'B' = B
    letter 'C' = C
    letter 'D' = D
    letter 'E' = E
    letter 'F' = F
    letter 'G' = G 
    
instance Deco Accidental where 
  deco = decoAccidental

-- accidental either all '#' or 
-- "#" (1), "x" (2), "x#" (3), "xx" (4), "xx#" (5), "xxx" (6), etc...
decoAccidental = choice [sharp, flat, nat]
  where 
    sharp         = plainsharp <|> doublesharp
    plainsharp    = Sharp <$> countChar '#'
    doublesharp   = mkDbl <$> countChar 'x' <*> option 0 (countChar '#')
    flat          = Flat  <$> countChar 'b'
    nat           = return Nat
    mkDbl dbl sgl = Sharp $ dbl * 2 + sgl
    countChar    = counting . char 

instance Deco ScaleDegreePattern where
  deco = decoScaleDegreePattern
  
decoScaleDegreePattern = ScaleDegreePattern <$> sepBy1 scaleDegree whiteSpace 
  where scaleDegree = flip (,) <$> decoAccidental <*> int   


--------------------------------------------------------------------------------
-- Affi instances
--------------------------------------------------------------------------------
instance Affi Pitch where
  affi (Pitch l o s c) | c == 0    = affi l . shows o 
                       | c > 0     = affi l . shows o . showChar '+' . shows c
                       | otherwise = affi l . shows o . shows c
    
    
instance Affi PitchLabel where 
    affi (PitchLabel l a) = affi l . affi a
        
-- print sharps as with muliple '#'s only
instance Affi Accidental where
  affi Nat        = showString ""
  affi (Sharp i)  = showString (replicate i '#')                          
  affi (Flat i)   = showString (replicate i 'b')     
  
  
instance Affi PitchLetter where
    affi = shows
    
      