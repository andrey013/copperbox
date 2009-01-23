{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.GlyphManagement
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- ...
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.GlyphManagement ( 
   Glyph,
   withGlyph  
) where


import Graphics.Rendering.FreeType.Internals.CBasicDataTypes () 
import Graphics.Rendering.FreeType.Internals.CGlyphManagement

import Control.Exception ( bracket )

import Foreign.Ptr ( nullPtr )

 
type Glyph = FT_glyph


withGlyph :: (Glyph -> IO a) -> IO a
withGlyph action = bracket newGlyph doneGlyph action
                            

newGlyph :: IO FT_glyph
newGlyph = return (FT_glyph nullPtr)


-- | Free the glyph on the C-side if it had been updated to 
-- point to something.
doneGlyph :: FT_glyph -> IO ()
doneGlyph (FT_glyph ptr) 
    | ptr == nullPtr = return ()
    | otherwise      = ft_done_glyph ptr

    


