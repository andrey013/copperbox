{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.Abc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Helpers for Abc construction / output.
--
--------------------------------------------------------------------------------


module MulleinHaskore.Abc where

-- import MulleinHaskore.System

import Mullein.AbcConvert
import Mullein.AbcDoc
import Mullein.AbcOutput
import Mullein.Core
import Mullein.SpellingMap

import Text.PrettyPrint.Leijen

{-

-- TODO follow LilyPond example

simpleAbc :: InstName -> Key -> MetricalSpec -> System -> Doc
simpleAbc name k m sys = unP $ scoreTemplate 1 "ABC -" k m abc_out
  where
    abc_motif = instMotif name k m sys
    abc_part  = linearPart abc_motif
    smap      = spellingMapE k
    abc_score = convertToAbc smap (unitNote m) abc_part
    abc_out   = generateAbc k (fst m) (repeat 4) abc_score




-- a version of makeSpellingMap that throws an error
spellingMapE :: Key -> SpellingMap
spellingMapE k = maybe failure id $ makeSpellingMap k [] where
    failure = error $ "Spelling map missing for " ++ show k

scoreTemplate :: Int -> String -> Key -> MetricalSpec -> AbcOutput -> P CtxField
scoreTemplate i score_title k m abc_output = 
    tunenum i +++ title score_title
              +++ meterinfo (fst m)
              +++ keyinfo  k
              +++ abcOutput abc_output

-}


--




