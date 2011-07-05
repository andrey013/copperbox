{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CSDDoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Doc combinators for the XML CSD file.
--
--------------------------------------------------------------------------------

module ZSnd.Core.CSDDoc
  (

    csdStartTag
  , csdEndTag
  , csdElem

  , commentLine

  , csound_synthesizer
  , cs_options
  , cs_instruments
  , cs_score 
  
  , cs_sco_table_stmt
  , cs_sco_inst_stmt

  ) where

import ZSnd.Core.ScoreInternal
import ZSnd.Core.Utils.FormatCombinators

csdStartTag :: String -> Doc
csdStartTag ss = angles $ text ss

csdEndTag :: String -> Doc
csdEndTag ss = angles $ text $ '/':ss

csdElem :: String -> Doc -> Doc
csdElem name d  = csdStartTag name `vconcat` d `vconcat` csdEndTag name

commentLine :: String -> Doc
commentLine ss = text $ '#': ss

csound_synthesizer :: Doc -> Doc -> Doc -> Doc
csound_synthesizer opts orch sco = 
    csdElem "CsoundSynthesizer" (opts `vconcat` orch `vconcat` sco)

cs_options :: [String] -> Doc
cs_options xs = csdElem "CsOptions" (vcat $ map text xs)

cs_instruments :: [Doc] -> Doc
cs_instruments xs = csdElem "CsInstruments" (vcat xs)


-- | Note - this does not add any terminators.
--
cs_score :: Doc -> Doc
cs_score d = csdElem "CsScore" d


cs_sco_table_stmt :: Int -> Double -> Int -> Int -> [CsoundValue] -> Doc
cs_sco_table_stmt ix t0 sz gennum args = 
    char 'f' <+> padr 5 (int ix) 
             <+> padl 5 (dtrunc t0) 
             <+> padl 5 (int sz)
             <+> padl 5 (int gennum) 
             <+> hsep (map (padl 5 . format) args)


cs_sco_inst_stmt :: Int -> Double -> Double -> [Doc] -> Doc
cs_sco_inst_stmt inst start dur args = 
    char 'i' <+> padr 5 (int inst)
             <+> padl 5 (dtrunc start) 
             <+> padl 5 (dtrunc dur)
             <+> hsep args

