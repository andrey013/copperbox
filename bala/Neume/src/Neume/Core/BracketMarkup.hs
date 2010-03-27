{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.BracketMarkup
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting for (simple) markup syntax.
--
--------------------------------------------------------------------------------

module Neume.Core.BracketMarkup
  ( 
    phraseMarkup

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.NoteList
import Neume.Core.SyntaxMarkup
import Neume.Core.Utils.OneList ( fromList )

-- Buffered
data Step a st = Yield   !st
               | Push  a !st
  deriving (Eq,Show)

-- little of the existing machinery can be used...

phraseMarkup :: MeterPattern 
             -> [SkipGlyph gly Duration] 
             -> MarkupPhrase (SkipGlyph gly Duration)
phraseMarkup mp = 
    MarkupPhrase . map mkBar . filter (not . null) . bufferedUnfold phi bar_len
  where
    bar_len = sum mp
    mkBar [] = error $ "phraseMarkup - empty bar" 
    mkBar xs = MarkupBar $ fromList xs

    phi n gly | n <= 0    = Yield     (bar_len - abs n)
              | otherwise = Push  gly (n - nmeasure gly) 

bufferedUnfold :: (st -> a -> Step a st) -> st -> [a] -> [[a]]
bufferedUnfold phi = step id where
  step f _  []     = [out f]
  step f st (x:xs) = case phi st x of 
                       Yield   st'  -> out f : step id st' xs
                       Push  a st'  -> step (snoc f a) st' xs
  
  out  f                = f []
  snoc f a              = f . (a:)