
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.AbcBackend
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.AbcBackend where

import Bala.Format.Output.OutputAbc
import Bala.Perform.AbcInterface

data Perform_Abc_State = Perform_Abc_State { 
    abc_unknown_st      :: ()
  }  
  deriving (Show)
  
data Perform_Abc_Env = Perform_Abc_Env {
    abc_unknown_env      :: ()
  }
  
infixl 7 *!
(*!) e oa   = maybe e (e !) oa



{-
renderPitch :: AbcPitch pch => ScPitch pch -> ProcessM pch dur (AbcNote) 
renderPitch (ScPitch pch) = 
    fn <$> pure (mkPitchLetter pch) <*> pure (mkAccidental pch) 
                                    -- <*> differOctaveSpec pch
  where
    fn pn oa oos = (pitch pn) *! oa *! oos   
    
-}      