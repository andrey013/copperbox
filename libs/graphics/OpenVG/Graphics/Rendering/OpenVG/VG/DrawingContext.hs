{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 4.3 (Forcing Drawing to Complete) 
-- of the OpenVG 1.0.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VG.DrawingContext (
  flush, finish
) where


import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgFlush, vgFinish ) 

flush :: IO ()
flush = vgFlush

finish :: IO ()
finish = vgFinish


 
