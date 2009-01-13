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

module Graphics.Rendering.FreeType.GlyphManagement () where


import Graphics.Rendering.FreeType.Internals.CBasicDataTypes () 
import Graphics.Rendering.FreeType.Internals.CGlyphManagement ()

{-
import Foreign.C.String ( withCString )
import Foreign.ForeignPtr ( newForeignPtr, finalizeForeignPtr, 
    withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
-- import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek )
-}

-- Note glyphs are allocated by the client not the library...
-- Values get stored in a glyph by FreeType copying them in from the 
-- current-glyph-in-the-glyphslot.

-- They actual contents of the glyph should be opaque to Haskell
-- as they contain pointers to the FreeType library and internal
-- /clazz/ data.


-- FreeType - FT_Glyph is a pointer.

-------------------------------------------------------------------------------- 


{-
initGlyph :: IO Glyph
initGlyph = do
    alloca $ \ptr -> 
-}
