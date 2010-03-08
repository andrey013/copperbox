{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.LilyPondFormat
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Formatting operations for bars (repeats etc.)
--
--------------------------------------------------------------------------------


module Neume.Extra.LilyPondFormat
  (
  
    simpleOutput

  ) where


import Neume.Core.SyntaxDoc

import Neume.Extra.LilyPondDoc

import Text.PrettyPrint.Leijen                  -- package: wl-pprint



simpleOutput :: LyPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . getLyBar) . getLyPhrase


-- Are you going to need an amalgamate monad?
-- if so it can e.g put "||" at the end of a passage if the end 
-- isn\'t a repeat.

-- ritornello :: Phrase -> [Bar] -> Doc

-- Check \parallelMusic ...  page 118 (pdf page 128)
-- notes are relative to the previous note in the voice - not 
-- the previous note in the input

-- accounting for overlays / parallel music takes the list-of Bars
-- to a forest of one-or-many Bars - [(OneMany Bar)]