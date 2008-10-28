--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Annotations
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common annotations for LilyPond and Abc 
--
--------------------------------------------------------------------------------

module HNotate.Annotations where

import HNotate.Document
import HNotate.NoteListDatatypes


-- Abc
upbow :: AnnoFun
upbow = (\d -> char 'u' <> d)

downbow :: AnnoFun
downbow = (\d -> char 'v' <> d)



-- LilyPond
fermata :: AnnoFun 
fermata = (<> command "fermata")


stringNum :: Int -> AnnoFun
stringNum i = (<> command (show i))


