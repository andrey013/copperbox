--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Document
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A specialized miniature PPrint like module.
-- Formatting is largely explicit so fitting is more limited.   
--------------------------------------------------------------------------------

module HNotate.Document (
  ODoc,
  
  empty, isEmpty,
  text, char, string,
  ( <> ), ( <+> ),
  output, formatted, unformatted 
  ) where

import qualified Data.Foldable as F
import Data.Sequence hiding (length, empty)
import Data.Monoid 
import Prelude hiding (null)


newtype ODoc = ODoc { getODoc :: Seq Particle }
  deriving Show
  
data Particle = Text Int String
              | Space           -- maybe rendered as space, maybe newline
              | LineBreak 
  deriving Show

spacejoin :: ODoc -> ODoc -> ODoc
spacejoin a b = ODoc $ (getODoc a |> Space) >< getODoc b

nobreakjoin :: ODoc -> ODoc -> ODoc
nobreakjoin a b = ODoc $ getODoc a  >< getODoc b

breakjoin :: ODoc -> ODoc -> ODoc
breakjoin a b = ODoc $ (getODoc a |> LineBreak) >< getODoc b

empty :: ODoc
empty = ODoc mempty

isEmpty :: ODoc -> Bool
isEmpty = null . getODoc 

text :: String -> ODoc
text s = ODoc $ singleton $ Text (length s) s

char :: Char -> ODoc
char c = text [c]


string :: String -> ODoc
string = build . map text . lines
  where
    build [x]     = x
    build (x:xs)  = foldl breakjoin x xs
    
    

infixr 6 <>,<+>
(<>) :: ODoc -> ODoc -> ODoc
a <> b | isEmpty a    = b
       | isEmpty b    = a
       | otherwise    = nobreakjoin a b

(<+>) :: ODoc -> ODoc -> ODoc
a <+> b | isEmpty a   = b
        | isEmpty b   = a
        | otherwise   = spacejoin a b
        
quickOutput :: ODoc -> ShowS
quickOutput = F.foldl fn id . getODoc
  where
    fn f (Text _ s)   = f . showString s
    fn f (Space)      = f . showChar ' '
    fn f (LineBreak)  = f . showChar '\n'


unformatted :: ODoc -> String 
unformatted = quickOutput `flip` "\n"

formatted :: Int -> Int -> ODoc -> String 
formatted indent_level width  = (output indent_level width) `flip` "\n"  

output :: Int -> Int -> ODoc -> ShowS
output indent_level width = 
    first . F.foldl out (indent,False,0) . getODoc
  where     
    out (f,sp,j) (Text i s)
          -- not a spacing break so we carry on on the same line 
          -- regardless of whether it actually 'fits'.                       
        | not sp           = (f . showString s, False, i+j) 
          -- spacing break - so it can either be a ' ' if the 
          -- line fits...           
        | fits (j + i)     = (f . space . showString s, False, i+j)
          -- ...or a newline if it doesn't.           
        | otherwise        = (f . nl . indent . showString s, False, i) 

          -- Space encountered - just change the state 
          -- (don't add it to the output)           
    out (f,_ ,j) (Space)      = (f, True, j+1)
    
          -- linebreak - add it to the doc and reset the state
    out (f,_ ,j) (LineBreak)  = (f . nl . indent, False, 0)
    
    -- helpers
    indent = showString (replicate indent_level ' ') 
    fits i = i <= width - indent_level


space, nl :: ShowS
space  = showChar ' '
nl     = showChar '\n'

first :: (a,b,c) -> a
first (a,_,_) = a
    
    

  
    