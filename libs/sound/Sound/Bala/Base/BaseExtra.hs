

module Sound.Bala.Base.BaseExtra where

import Control.Applicative hiding (many, optional)
import Control.Monad (ap)
import Data.Char
import Data.List (find)
import Text.ParserCombinators.ReadP

instance Applicative ReadP where
  pure = return
  (<*>) = ap
  
elements :: Read a => String -> [a]
elements = map read . words

  
token :: ReadP a -> ReadP a
token p = p <* skipSpaces

zam :: (a -> a -> b) -> [a] -> [b]
zam f (x:y:ys) = f x y : zam f (y:ys)
zam f _        = []


{-

elt :: Show a => [(a,String)] -> a
elt xs = case find (\(_,s) -> s=="") xs of
          Just (a,_) -> a
          Nothing -> error $ show xs



parse1 :: (Show a, ElementParse a) => String -> a
parse1 s = elt $ readP_to_S parseElt s

parseMany1WhiteSep :: ElementParse a => ReadS [a]
parseMany1WhiteSep s = readP_to_S (many1 $ token parseElt) s

class ElementParse a where 
  parseElt :: ReadP a 
  
-}
  