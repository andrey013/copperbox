{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common utils...
--
--------------------------------------------------------------------------------

module Mullein.Utils 
  ( 
  
    Groupoid(..)

  , divModR

  -- special zips, unfolds etc.
  , longZipWith
  , endoLongZip
  , anaMap
  , anaSt
  , unfoldr2
  , unfoldrMonoid
  , unfoldlMonoid
  , genUnfold
  , genUnfold2
  , unfoldrMonoid2
  
  , oo
  , ooo 
  , oooo

  , prod
  , enumFromCyc
  , nextOf
  , rational

  -- extra pretty printers
  , doclines
  , dblangles
  , nextLine
  , sglLine
  , doubleQuote
  , renderDocEighty
  , emptyDoc

  ) where

import Control.Applicative ( Applicative(..) )
import Control.Monad.State
import Data.List ( unfoldr )
import Data.Monoid
import Data.Ratio

import Text.PrettyPrint.Leijen hiding ( empty, rational )
import qualified Text.PrettyPrint.Leijen as PP

class Groupoid a where
  gappend :: a -> a -> a


instance Applicative (State s) where
  pure  = return
  (<*>) = ap


--------------------------------------------------------------------------------
-- divMod (with rounding) for rationals 

-- check - 8.0 `divModR` 0.75

-- prop_mod_postive a b = let (_,md) = a `divModR` b in signum md == 1

divModR :: (Integral b) => Ratio b -> Ratio b -> (b, Ratio b)
divModR a b = let a1 = a / b; a2 = floor a1 in (a2, a-((a2%1)*b))


--------------------------------------------------------------------------------


longZipWith :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
longZipWith f g h as bs = step as bs where
    step (x:xs) (y:ys) = f x y : step xs ys
    step (x:xs) []     = g x : step xs []
    step []     (y:ys) = h y : step [] ys
    step []     []     = []
 

endoLongZip :: (a -> a -> a) -> [a] -> [a] -> [a]
endoLongZip f (x:xs) (y:ys) = f x y : endoLongZip f xs ys
endoLongZip _ []     ys     = ys
endoLongZip _ xs     []     = xs



-- anaMap is the unfold analogue of accumMapL
-- we can signal exhaustion early by the Maybe type                
anaMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
anaMap _ s0 []     = ([],s0)     
anaMap f s0 (x:xs) = case (f x s0) of
    Nothing       -> ([],s0)
    Just (a,st)   -> (a:as,b) where (as,b) = anaMap f st xs



-- variant of /apomorphism/, but we return the final state 
-- rather than running a flush function on it
anaSt :: (st -> Maybe (a,st)) -> st -> ([a],st)
anaSt f s0 = case (f s0) of
    Nothing     -> ([],s0)
    Just (a,st) -> (a:as,b) where (as,b) = anaSt f st 


unfoldr2 :: (s1 -> s2 -> Maybe (a,s1,s2)) -> s1 -> s2 -> [a]
unfoldr2 f s1 s2 = case f s1 s2 of
    Nothing          -> []
    Just (a,s1',s2') -> a : unfoldr2 f s1' s2'


unfoldrMonoid :: Monoid a => (st -> Maybe (a,st)) -> a -> st -> a 
unfoldrMonoid f a0 s = step $ f s where
    step Nothing        = a0
    step (Just (a,s'))  = a `mappend` step (f s')

unfoldrMonoid2 :: Monoid a 
               => (s1 -> s2 -> Maybe (a,s1,s2)) -> s1 -> s2 -> a
unfoldrMonoid2 f s1 s2 = case f s1 s2 of
    Nothing          -> mempty
    Just (a,s1',s2') -> a `mappend` unfoldrMonoid2 f s1' s2'
 


unfoldlMonoid :: Monoid a => (st -> Maybe (a,st)) -> a -> st -> a 
unfoldlMonoid f a0 s = step $ f s where
    step Nothing        = a0
    step (Just (a,s'))  = let a' = step (f s') in a' `seq`  a' `mappend` a


genUnfold :: (st -> Maybe (a,st)) -> (a -> a -> a) -> a -> st -> a
genUnfold f g a0 s = step $ f s where
    step Nothing        = a0
    step (Just (a,s'))  = a `g` step (f s')

-- genUnfold with 2 states...
genUnfold2 :: (s1 -> s2 -> Maybe (a,s1,s2)) -> (a -> a -> a) -> a -> s1 -> s2 -> a
genUnfold2 f g a0 s t = step $ f s t where
    step Nothing          = a0
    step (Just (a,s',t')) = a `g` step (f s' t')



-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g    
    

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d) 
prod f g (a,b) = (f a,g b)

enumFromCyc :: (Bounded a, Enum a, Eq a) => a -> [a]
enumFromCyc a = a : (unfoldr f $ nextOf a)
  where 
    f x | x == a    = Nothing
        | otherwise = Just (x,nextOf x)
        
nextOf :: (Bounded a, Eq a, Enum a) => a -> a  
nextOf x | x == maxBound = minBound
         | otherwise     = succ x
                 
rational :: Integral a => a -> a -> Rational
rational a b = fromIntegral a % fromIntegral b

---------------------------------------------------------------------------------
-- PPrint extras 

-- This function is primarily for Abc bar printing, where the number
-- of bars on a line in the input score is reflected by the number of
-- bars on a line in the output.

doclines :: [Int] -> [Doc] -> Doc
doclines = vsep `oo` step where
    step _      []  = []
    step []     ds  = [hsep ds]
    step (n:ns) ds  = hsep ls : step ns rs where (ls,rs) = splitAt n ds

dblangles :: Doc -> Doc 
dblangles = enclose (text "<< ") (text " >>")


-- an alternative to (<$>) when Control.Applicative is alos imported
infixr 5 `nextLine`
nextLine :: Doc -> Doc -> Doc 
nextLine = (<$>)

sglLine :: Doc -> Doc 
sglLine d = d <> line


doubleQuote :: String -> Doc
doubleQuote = dquotes . string

renderDocEighty :: Doc -> String
renderDocEighty = (displayS `flip` []) . renderPretty 0.8 80

emptyDoc :: Doc
emptyDoc = PP.empty