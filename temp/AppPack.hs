
-- applicative Packrat (??)

module AppPack where

import Control.Applicative


data Result v = Parsed v Derivs
              | NoParse
              

data Derivs = Derivs
  { dvAdditive  :: Result Int,
    dvMultitive :: Result Int,
    dvPrimary   :: Result Int,
    dvDecimal   :: Result Int, 
    dvChar      :: Result Char }
    
    
    
newtype Par a = P { par :: (Derivs -> Result a) }

unP (P a) = a


instance Functor Par where
  fmap f (P g) = P $ \d -> 
    case g d of 
      NoParse -> NoParse
      Parsed v d' -> Parsed (f v) d'
                                      
                                      
instance Applicative Par where
  pure x = P (\d -> Parsed x d)
  (<*>) = stary
  
  
stary :: Par (a -> b) -> Par a -> Par b
stary (P p) (P q) = P $ \d -> 
  case p d of 
    NoParse -> NoParse
    Parsed f d' -> case q d' of
                     NoParse -> NoParse
                     Parsed v d'' -> Parsed (f v) d''
   
infixl 3 </>


(</>) :: Par a -> Par a -> Par a
(P p) </> (P q) = P $ \d -> case p d of 
                              ans@(Parsed _ _) -> ans
                              NoParse -> q d

parse :: String -> Derivs
parse s = d where
  d    = Derivs add mult prim dec chr
  add  = (unP pAdditive) d
  mult = (unP pMultitive) d
  prim = (unP pPrimary) d
  dec  = (unP pDecimal) d
  chr  = case s of
           (c:s') -> Parsed c (parse s')
           [] -> NoParse


eval :: Par a -> String -> a
eval (P f) s = case f (parse s) of
  Parsed v rem -> v
  _ -> error "Parse error"    

pAdditive  :: Par Int
pAdditive  = (+) <$> pPrimary <* pChar '+' <*> pAdditive </> pMultitive 

pMultitive :: Par Int
pMultitive = (*) <$> pPrimary <* pChar '*' <*> pMultitive </> pPrimary 

pPrimary   :: Par Int
pPrimary   = id <$ pChar '(' <*> pAdditive <* pChar ')' </> pDecimal


-- Parse a decimal digit
pDecimal   :: Par Int
pDecimal   = P $ \d -> case dvChar d of
  Parsed '0' d' -> Parsed 0 d'
  Parsed '1' d' -> Parsed 1 d'
  Parsed '2' d' -> Parsed 2 d'
  Parsed '3' d' -> Parsed 3 d'
  Parsed '4' d' -> Parsed 4 d'
  Parsed '5' d' -> Parsed 5 d'
  Parsed '6' d' -> Parsed 6 d'
  Parsed '7' d' -> Parsed 7 d'
  Parsed '8' d' -> Parsed 8 d'
  Parsed '9' d' -> Parsed 9 d'
  _ -> NoParse

pChar :: Char -> Par Char  
pChar c = P $ \d -> case dvChar d of
  Parsed c' d' -> if (c == c') then (Parsed c' d') else NoParse
  _ -> NoParse
  
  
demo01 = eval pAdditive "2*(3+4)"

  
                    