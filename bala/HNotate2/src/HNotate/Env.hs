--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Env
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output generation is /context sensitive/ - for instance both Abc and 
-- LilyPond files declare a time signature before a note list - the time 
-- signature is both printed (it is obviously important to the performer), 
-- but it is also used internally to segment note lists into bars and beam 
-- groups. 
--
-- The Env reprepresents these contextual paramters, different sets are 
-- relevant to Abc and LilyPond.
--
--------------------------------------------------------------------------------

module HNotate.Env where

import HNotate.Duration
import HNotate.MusicRepDatatypes


class MetricalEnv env where
    get_anacrusis :: env -> Duration
    get_meter     :: env -> Meter



  
  