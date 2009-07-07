{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
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
  
  -- * Forcing drawing to complete
  flush, 
  finish
) where


import Graphics.Rendering.OpenVG.VG.CFunDecls ( 
        vgFlush, vgFinish ) 

--------------------------------------------------------------------------------
--  Forcing drawing to complete

-- | @flush@ - corresponds directly to the OpenVG call @vgFlush@.
flush :: IO ()
flush = vgFlush

-- | @finish@ - corresponds directly to the OpenVG call @vgFinish@.
finish :: IO ()
finish = vgFinish


 
