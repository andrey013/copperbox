{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.ShivaExtensions
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
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

-- | 'flush' ensures all the outstanding drawing requests on the 
-- current context are completed. The call may return before the 
-- actual drawing takes place.
--
-- 'flush' corresponds to the OpenVG call @vgFlush@.
-- 
flush :: IO ()
flush = vgFlush

-- | 'finish' forces all the outstanding drawing requests on the 
-- current context are performed. The call returns when drawing is
-- completed.
--
-- 'finish' corresponds to the OpenVG call @vgFinish@.
--
finish :: IO ()
finish = vgFinish


 
