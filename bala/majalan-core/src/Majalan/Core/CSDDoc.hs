{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.CSDDoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Doc combinators for the XML-like CSD file.
--
--------------------------------------------------------------------------------

module Majalan.Core.CSDDoc
  (

    csdStartTag
  , csdEndTag
  , csdElem

  , commentLine

  , csound_synthesizer
  , cs_options
  , cs_instruments
  , cs_score 
  

  ) where

import Majalan.Core.Utils.DocExtras

import Text.PrettyPrint.HughesPJ

csdStartTag :: String -> Doc
csdStartTag ss = angles $ text ss

csdEndTag :: String -> Doc
csdEndTag ss = angles $ text $ '/':ss

csdElem :: String -> Doc -> Doc
csdElem name d  = vconcat [ csdStartTag name, d, csdEndTag name ]

commentLine :: String -> Doc
commentLine ss = text $ '#': ss

csound_synthesizer :: Doc -> Doc -> Doc -> Doc
csound_synthesizer opts orch sco = 
    csdElem "CsoundSynthesizer" (vconcat [ opts, orch, sco ])

cs_options :: [String] -> Doc
cs_options xs = csdElem "CsOptions" (vcat $ map text xs)

cs_instruments :: [Doc] -> Doc
cs_instruments xs = csdElem "CsInstruments" (vcat xs)


-- | Note - this does not add any terminators.
--
cs_score :: Doc -> Doc
cs_score d = csdElem "CsScore" d

