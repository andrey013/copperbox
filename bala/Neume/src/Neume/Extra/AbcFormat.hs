{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.AbcFormat
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


module Neume.Extra.AbcFormat
  (

    simpleOutput

  ) where

import Neume.Core.SyntaxDoc
import Neume.Extra.AbcDoc

import Text.PrettyPrint.Leijen

-- | Output ABC, four bars printed on each line. 
simpleOutput :: AbcPhrase -> Doc
simpleOutput = four . map ((<+> singleBar) . getAbcBar) . getAbcPhrase


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs

