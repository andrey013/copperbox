{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Printing with /join-strings/.
--
-- Note - JoinPrint is just a formatter and not a \'pretty printer\'. 
-- No line fitting takes place - lines are printed exactly as 
-- they are specified. 
--
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint
  ( 
    module Text.PrettyPrint.JoinPrint.Core
  , module Text.PrettyPrint.JoinPrint.HexDump  
  ) where

import Text.PrettyPrint.JoinPrint.Core
import Text.PrettyPrint.JoinPrint.HexDump

