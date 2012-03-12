{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirectoryMetrics.Pretty
-- Copyright   :  (c) Stephen Peter Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Pretty printer
--
-----------------------------------------------------------------------------


module DirectoryMetrics.Pretty
  (

    ppDateTime
  , ppCountingMap
  , ppSizeMetrics
  , ppBothMetrics

  ) where

import DirectoryMetrics.CountingMap
import DirectoryMetrics.HierSyntax
import DirectoryMetrics.SizeMetrics
import DirectoryMetrics.StructureMetrics

import Text.PrettyPrint.SPretty

import qualified Data.IntMap as IM



ppDateTime :: DateTime -> Doc
ppDateTime a = year <+> day
  where
    year = intersperse (char '/') [ nat4 $ dt_year a
                                  , nat2 $ dt_month a
                                  , nat2 $ dt_day a ]
    day  = nat2 (dt_hour a) <> colon <> nat2 (dt_min a)


ppCountingMap :: CountingMap -> Doc
ppCountingMap = vsep . map pp . IM.assocs . toIntMap
  where
    pp (k,v) = padShowL 2 ' ' k <> colon <+> int v


ppSizeMetrics :: SizeMetrics -> Doc
ppSizeMetrics a = 
        text "Size Metrics..."       
    $+$ padStringR 15 ' ' "File count:" <+> int (sz_file_count a)
    $+$ padStringR 15 ' ' "Subdir count:"  <+> int (sz_subdir_count a)
    $+$ text "File count frequencies:"
    $+$ ppCountingMap (sz_file_counts a)
    $+$ text "Subdir count frequencies:"
    $+$ ppCountingMap (sz_subdir_counts a)
   
ppStructMetrics  :: StructMetrics -> Doc
ppStructMetrics a = 
        text "Structure Metrics..."       
    $+$ padStringR 15 ' ' "Max depth:" <+> int (str_max_depth a)


ppBothMetrics :: (SizeMetrics,StructMetrics) -> Doc
ppBothMetrics (a,b) = ppSizeMetrics a $+$ ppStructMetrics b