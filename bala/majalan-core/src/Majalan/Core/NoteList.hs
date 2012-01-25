{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty print note-lists using the carry symbol @(.)@ when
-- appropriate.
--
--------------------------------------------------------------------------------

module Majalan.Core.NoteList
  (

    ColumnSpecs
  , columnSpecs


  , CsEvent(..)
  , printEvents
 
  ) where

import Majalan.Core.Utils.DocExtras

-- import Control.Applicative hiding ( empty )
import qualified Data.IntMap as IM
import Text.PrettyPrint.HughesPJ


data CsEvent = CsEvent 
      { instr_num   :: Int
      , onset_time  :: Double
      , event_args  :: [Double]
      }




type ColumnSpecs = IM.IntMap [(Int,Int)]


columnSpecs :: [(Int,[(Int,Int)])] -> ColumnSpecs
columnSpecs = IM.fromList





-- | For printing we have a tolerance of 0.0001 decimal places 
-- for equality.
--
tEq :: Double -> Double -> Bool
tEq a b = (abs (a-b)) < 0.0001

decimalZ :: Double -> Doc
decimalZ = decimal 3 6

printEvents :: [CsEvent] -> ColumnSpecs -> Doc
printEvents []     _    = empty
printEvents (c:cs) cols = printEvent1 c cols $+$ step cs c
  where
    step []     _   = empty
    step (a:as) b   = printEvent a b cols $+$ step as a 




printEvent1 :: CsEvent -> ColumnSpecs -> Doc
printEvent1 (CsEvent i1 ot ds1) cols = 
    char 'i' <> int i1 <+> decimalZ ot <+> (hsep $ printColumns ds1 icols)
  where
    icols  = IM.findWithDefault [] i1 cols


-- | Never carry duration, just carry arguments.
-- 
printEvent :: CsEvent -> CsEvent -> ColumnSpecs -> Doc
printEvent (CsEvent i1 ot ds1) (CsEvent i2 _ ds2) cols
    | i1 == i2  = prefix <+> (hsep $ printDiffColumns ds1 ds2 icols)
    | otherwise = prefix <+> (hsep $ printColumns ds1 icols)
  where
    prefix = char 'i' <> int i1 <+> decimalZ ot
    icols  = IM.findWithDefault [] i1 cols


printDiffColumns :: [Double] -> [Double] -> [(Int,Int)] -> [Doc]
printDiffColumns (d:ds) (a:as) ((p,w):cs)
    | d `tEq` a = paddedTextR "." w : printDiffColumns ds as cs
    | otherwise = decimal p w d : printDiffColumns ds as cs

printDiffColumns (d:ds) (a:as) []         
    | d `tEq` a = paddedTextR "." 6 : printDiffColumns ds as []
    | otherwise = decimalZ d : printDiffColumns ds as []

printDiffColumns ds     []     cs         = printColumns ds cs
    
printDiffColumns []     _      _          = []


-- | This is a long zip with a default width of 5.
--
printColumns :: [Double] -> [(Int,Int)] -> [Doc]
printColumns (d:ds) ((p,w):cs) = decimal p w d : printColumns ds cs
printColumns ds     []         = map decimalZ ds
printColumns []     _          = []








