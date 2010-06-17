{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.AbcDoc
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for ABC.
--
--------------------------------------------------------------------------------


module Neume.Extra.AbcDoc
  (

  
  -- * Fields
    field
  , tunenum  
  , title
  , book 
  , composer
  , unitDuration
  , key
  , meter
  , tempo

  ) where

import Neume.Core.Duration

import Text.PrettyPrint.Leijen          -- package: wl-pprint




-- ** Fields

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

-- | @K field@ - key.
-- Note - the key parameter should correspond to the key used to 
-- generate the ABC tune.
--
key :: String -> Doc
key = field 'K' . text  


-- | @L field@ - unit note length.
-- Note - the unit note length parameter should correspond to the 
-- duration used by the 'durationRewrite' step when generating 
-- the ABC tune.
--
unitDuration :: Duration -> Doc
unitDuration drn = field 'L' frac where
  frac = let (n,d) = extentComponents drn in int n <> char '/' <> int d  



-- | @M field@ - meter.
-- Note - the meter parameter should correspond to the meter 
-- component of the @MetricalSpec@ used to generate the ABC tune. 
-- Also the meter should come before the key field in a tune, 
-- otherwise meter may get interpreted as a midtune field.
meter :: String -> Doc
meter = field 'M' . text

-- | @Q field@ - tempo.
-- Note - the range of ABC tempos is very wide, therefore no 
-- attempt is made to encapsulate /tempo/ as an abstract datatype. 
-- Instead tempo is just a string literal. For genuine literals 
-- (e.g. Andante) make sure they are double-quote first.
tempo :: String -> Doc
tempo = field 'Q' . text




