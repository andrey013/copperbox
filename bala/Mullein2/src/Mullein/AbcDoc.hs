{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcDoc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for ABC.
--
--------------------------------------------------------------------------------


module Mullein.AbcDoc
  (
    -- * Fields

    field
  , tunenum  
  , title
  , book 
  , composer
  , meter
  , tempo
  , key

  ) where

import Text.PrettyPrint.Leijen



field :: Char -> Doc -> Doc
field ch d = char ch <> colon <> d


-- | @X field@ - reference \/ tune number.
tunenum :: Int -> Doc
tunenum = field 'X' . int

-- | @T field@ - title. 
title :: String -> Doc
title = field 'T' . text

-- | @B field@ - book.
book :: String -> Doc
book = field 'B' . text

-- | @C field@ - composer name.
composer :: String -> Doc
composer = field 'C' . text

-- | @M field@ - meter.
-- Note - the meter parameter should correspond to the meter component
-- of the @MetricalSpec@ used to generate the ABC tune.
meter :: String -> Doc
meter = field 'M' . text

-- | @Q field@ - tempo.
-- Note - the range of ABC tempos is very wide, therefore no attempt
-- is made to encapsulate /tempo/ as an abstract datatype. Instead
-- tempo is just a string literal. If you want to use a genuine literal
-- (e.g. Andante) make sure you double-quote it first.
tempo :: String -> Doc
tempo = field 'Q' . text

-- | @K field@ - key.
-- Note - the key parameter should correspond to the key used to 
-- generate the ABC tune.
key :: String -> Doc
key = field 'K' . text  




