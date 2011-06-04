{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.CSDDoc
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

module ZScore.CSDDoc
  (

    csdStartTag
  , csdEndTag
  , csdElem

  , commentLine

  , cs_options
  , cs_instruments
  , cs_score 

  ) where


import ZScore.Utils.FormatCombinators

csdStartTag :: String -> Doc
csdStartTag ss = angles $ text ss

csdEndTag :: String -> Doc
csdEndTag ss = angles $ text $ '/':ss

csdElem :: String -> Doc -> Doc
csdElem name d  = csdStartTag name `vconcat` d `vconcat` csdEndTag name

commentLine :: String -> Doc
commentLine ss = text $ '#': ss

cs_options :: [String] -> Doc
cs_options xs = csdElem "CsOptions" (vcat $ map text xs)

cs_instruments :: [Doc] -> Doc
cs_instruments xs = csdElem "CsInstruments" (vcat xs)


-- | Note - this terminates with an @e@.
--
cs_score :: [Doc] -> Doc
cs_score xs = csdElem "CsScore" (vcat xs `vconcat` char 'e' )
