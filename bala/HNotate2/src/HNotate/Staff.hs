{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Staff
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Rendering is done in /lines/ of music a line is equivalent to a voice
-- in Abc or a staff in LilyPond. 
-- We use the LilyPond term /staff/ for a line.
--
--------------------------------------------------------------------------------



module HNotate.Staff where

import HNotate.Cardinal


newtype Staff a = Staff { getStaff :: [Overlay a] }

-- Follow the Abc style when voice overlays are grouped in whole bars.
type Overlay a         = Cardinal (Bar a)

type BeamGroup a = Cardinal a

data Bar a  = Bar [BeamGroup a] | TiedBar a [BeamGroup a]
                      

                        


