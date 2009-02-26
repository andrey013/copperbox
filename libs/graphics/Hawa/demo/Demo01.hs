
--------------------------------------------------------------------------------
-- |
-- Module      :  Demo01
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Vector drawing... 
--
--------------------------------------------------------------------------------

module Demo01 where

import Graphics.Hawa
import Prelude hiding ( cycle )


(#) a f = f a

demo01 = run "out1.ps" $ draw $ [line1,line2] where
    
    line1 = line (100,100) (200,200) & line (250,250) (300,300)
    line2 = curve (400,100) (360,200) (400,400) # cycle
    



