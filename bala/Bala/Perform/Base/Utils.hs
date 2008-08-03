
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Base.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common utility functions.
--
--------------------------------------------------------------------------------

module Bala.Perform.Base.Utils (
  simpledoc, displaySimple
  ) where

import Text.PrettyPrint.Leijen

simpledoc :: Doc -> SimpleDoc
simpledoc d = renderPretty 0.8 80 (pretty d)

displaySimple :: Doc -> String
displaySimple a = displayS (simpledoc $ a <$> empty) ""




