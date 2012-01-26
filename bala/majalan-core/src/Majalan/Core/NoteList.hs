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
  , CsValue(..)
  , printEvents
 
  ) where

import Majalan.Core.Utils.DocExtras

-- import Control.Applicative hiding ( empty )
import qualified Data.IntMap as IM
import Text.PrettyPrint.HughesPJ


data CsEvent = CsEvent 
      { instr_num   :: Int
      , onset_time  :: Double
      , event_args  :: [CsValue]
      }
  deriving (Eq,Ord,Show)


data CsValue = I Int | D Double
  deriving (Ord,Show)
  

instance Eq CsValue where
  I i == I j = i == j
  D d == D e = d `tEq` e
  _   == _   = False


type ColumnSpecs = IM.IntMap [(Int,Int)]


columnSpecs :: [(Int,[(Int,Int)])] -> ColumnSpecs
columnSpecs = IM.fromList





-- | For printing we have a tolerance of 0.0001 decimal places 
-- for equality.
--
tEq :: Double -> Double -> Bool
tEq a b = (abs (a-b)) < 0.0001



valueZ :: CsValue -> Doc
valueZ (D d) = doubleZ d
valueZ (I i) = intColumn 6 i

value :: (Int,Int) -> CsValue -> Doc
value (p,w) (D d) = decimal p w d
value (_,w) (I i) = intColumn w i

doubleZ :: Double -> Doc
doubleZ = decimal 3 6

printEvents :: [CsEvent] -> ColumnSpecs -> Doc
printEvents []     _    = empty
printEvents (c:cs) cols = printEvent1 c cols $+$ step cs c
  where
    step []     _   = empty
    step (a:as) b   = printEvent a b cols $+$ step as a 




printEvent1 :: CsEvent -> ColumnSpecs -> Doc
printEvent1 (CsEvent i1 ot ds1) cols = 
    char 'i' <> int i1 <+> doubleZ ot <+> (hsep $ printColumns ds1 icols)
  where
    icols  = IM.findWithDefault [] i1 cols


-- | Never carry duration, just carry arguments.
-- 
printEvent :: CsEvent -> CsEvent -> ColumnSpecs -> Doc
printEvent (CsEvent i1 ot ds1) (CsEvent i2 _ ds2) cols
    | i1 == i2  = prefix <+> (hsep $ printDiffColumns ds1 ds2 icols)
    | otherwise = prefix <+> (hsep $ printColumns ds1 icols)
  where
    prefix = char 'i' <> int i1 <+> doubleZ ot
    icols  = IM.findWithDefault [] i1 cols


printDiffColumns :: [CsValue] -> [CsValue] -> [(Int,Int)] -> [Doc]
printDiffColumns (v:vs) (a:as) (s:ss)
    | v == a    = paddedTextR "." (snd s) : printDiffColumns vs as ss
    | otherwise = value s v : printDiffColumns vs as ss

printDiffColumns (v:vs) (a:as) []
    | v == a    = paddedTextR "." 6 : printDiffColumns vs as []
    | otherwise = valueZ v : printDiffColumns vs as []

printDiffColumns ds     []     cs         = printColumns ds cs
    
printDiffColumns []     _      _          = []


-- | This is a long zip with a default width of 5.
--
printColumns :: [CsValue] -> [(Int,Int)] -> [Doc]
printColumns (v:vs) (s:ss) = value s v : printColumns vs ss
printColumns vs     []     = map valueZ vs
printColumns []     _      = []









