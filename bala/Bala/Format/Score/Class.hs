--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Score.Class
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Typeclass so that the parameters of a Score (pitch and duration) can 
-- be printed. 
-- We may not want the default Show instance, Certainly in the case of 
-- Bala.Pitch and Bala.Duration the output is too verbose.
--
--------------------------------------------------------------------------------

module Bala.Format.Score.Class (  
  Printable(..)
  ) where


-- We might not want the default show instance    
class Printable a where stringrep :: a -> String
