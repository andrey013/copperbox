{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.CoreAdditions
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Additions due to be added to Wumpus.Core
-- (or maybe not...)
--
--------------------------------------------------------------------------------


module Wumpus.Extra.CoreAdditions
  (

    zeroPicture

  ) where


import Wumpus.Core

-- Not sure about this - it is useful, but it might 
-- have bad 'semantics' 
--
zeroPicture :: Num u => Picture u
zeroPicture = blankPicture (BBox zeroPt zeroPt)

