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
  
  -- * Groupoid class 
    Groupoid(..)

  -- * Functions
  , divModR
  , makeRational
  , anaMap

  -- * Specs!  
  , oo
  , ooo 
  , oooo

  , prod

  -- extra pretty printers
  , doclines
  , dblangles
  , nextLine
  , sglLine
  , doubleQuote
  , renderDocEighty
  , emptyDoc
  , spacedBraces

  ) where

import Data.Ratio

import Text.PrettyPrint.Leijen hiding ( empty, rational )
import qualified Text.PrettyPrint.Leijen as PP

class Groupoid a where
  gappend :: a -> a -> a



--------------------------------------------------------------------------------
-- divMod (with rounding) for rationals 

-- check - 8.0 `divModR` 0.75

-- prop_mod_postive a b = let (_,md) = a `divModR` b in signum md == 1

divModR :: (Integral b) => Ratio b -> Ratio b -> (b, Ratio b)
divModR a b = let a1 = a / b; a2 = floor a1 in (a2, a-((a2%1)*b))


                 
makeRational :: Integral a => a -> a -> Rational
makeRational a b = fromIntegral a % fromIntegral b

--------------------------------------------------------------------------------
 


-- | @anaMap@ is the unfold analogue of accumMapL.
-- We can signal exhaustion early by the Maybe type.                
anaMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
anaMap _ s0 []     = ([],s0)     
anaMap f s0 (x:xs) = case (f x s0) of
    Nothing       -> ([],s0)
    Just (a,st)   -> (a:as,b) where (as,b) = anaMap f st xs



-- 'specs'

oo :: (c -> d) -> (a -> b -> c) -> a -> b -> d
oo f g = (f .) . g

ooo :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
ooo f g = ((f .) .) . g

oooo :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
oooo f g = (((f .) .) .) . g    
    

prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d) 
prod f g (a,b) = (f a,g b)

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

spacedBraces :: Doc -> Doc
spacedBraces = enclose (text "{ ") (text " }")