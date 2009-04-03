{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DocLilyPond
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output functions for /doc mode/ LilyPond 
--
--------------------------------------------------------------------------------

module HNotate.DocLilyPond where

import HNotate.DocBase

type LyOutput = DocK LyEnv Doc

version :: LyOutput
version = command "version" <+> dblquotes (text "2.10.33")

header :: LyOutput -> LyOutput 
header d = onNewline $ command "header" <+> (bracesLines d)


--------------------------------------------------------------------------------

bracesLines :: Doc -> Doc
bracesLines d = lbrace <+> (indent 2 d) <+> rbrace



    