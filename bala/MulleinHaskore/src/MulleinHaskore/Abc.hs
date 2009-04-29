{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  HaskoreMullein
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  Flexible instances, mptc.
--
-- An interface for Haskore to Mullein
--
--------------------------------------------------------------------------------


module MulleinHaskore.Abc where

import MulleinHaskore.System

import Mullein.AbcConvert
import Mullein.AbcDoc
import Mullein.AbcOutput
import Mullein.Core
import Mullein.CoreTypes
import Mullein.Score
import Mullein.SpellingMap

import Text.PrettyPrint.Leijen

simpleAbc :: InstName -> Key -> MetricalSpec -> System -> Doc
simpleAbc name k m sys = unP $ scoreTemplate 1 "ABC -" k m abc_out
  where
    abc_motif = instMotif name k m sys
    abc_part  = linearPart abc_motif
    smap      = spellingMapE k
    abc_score = convertToAbc smap (unitNote m) abc_part
    abc_out   = generateAbc k (fst m) (repeat 4) abc_score

linearPart :: Motif -> Part
linearPart m = part [phrase m]

instMotif :: InstName -> Key -> MetricalSpec -> System -> Motif
instMotif name k m sys = motif k m ovs where
    ovs = makeOverlays name sys

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



--




