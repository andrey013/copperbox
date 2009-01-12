{-# LANGUAGE ForeignFunctionInterface   #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.FreeType.BaseInterface
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Binding to part of the FreeType2 /Base Interface/.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.FreeType.BaseInterface (
  
  -- * initialize and free a FreeType /library/.
  initFreeType,
  doneFreeType,
  
  -- * newFace
  newFace,
  doneFace,
  
  -- * Face field accessors
  numFaces, 
  faceIndex,
  numGlyphs,
  familyName, 
  styleName,
  
  Bitmap(..),
  glyphSlotBitmap,
  gsBitmapLeft,
  gsBitmapTop, 
  
  -- * ...
  selectSize,
  
  setCharSize,
  setPixelSizes,
  
  -- * loading ...
  LoadFlag(..), -- re-export from Internal
  loadGlyph,
  loadChar,
  RenderMode(..),
  renderCurrentGlyph, -- 
  
  postscriptName,
  
  -- * FreeType library version number
  VersionNumber, 
  libraryVersion,
) where

import Graphics.Rendering.FreeType.FixedPrecision
import Graphics.Rendering.FreeType.Internals.CBaseInterface
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.Wrappers 

import Control.Applicative ( (<$>) )
import Control.Monad
import Data.Bits ( (.|.) )
import Foreign.C.String ( withCString, peekCString )
import Foreign.ForeignPtr ( newForeignPtr, finalizeForeignPtr, 
    withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
-- import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek )



-------------------------------------------------------------------------------- 
  
-- | Initialize a handle to the FreeType library, @FT_Init_FreeType@ 
-- allocates memory for the library on the C side
initFreeType :: IO FTlibrary
initFreeType =
  alloca $ \ptrptr -> do 
  ec <- ft_init_freetype ptrptr
  case ec of
    0 -> do fin <- mkDoneLibrary freeLibrary_
            ptr <- peek ptrptr
            p   <- newForeignPtr fin ptr
            return (FTlibrary p)
    _ -> fail ("initFreeType: failed to initialize, error " ++ show ec)


-- | Free the library handle, internally @FT_Done_FreeType@ will get called
-- to free the memory on the C side. 
doneFreeType :: FTlibrary -> IO ()
doneFreeType (FTlibrary h) = finalizeForeignPtr h

-- freeLibrary_ drops the return code from @ft_done_freetype@ so 
-- that the type signature is compatible with @FinalizerPtr@.
-- Note the argumnent is @FT_Library@ and not @FTlibrary@.
freeLibrary_ :: FT_Library -> IO ()
freeLibrary_ p = ft_done_freetype p >> return ()


--------------------------------------------------------------------------------
-- New face

newFace :: FTlibrary -> FilePath -> Int -> IO FTface
newFace (FTlibrary lib) path_s idx = 
    withForeignPtr lib $ \h ->
        withCString path_s $ \path ->  
            alloca $ \ptrptr -> do 
            ec <- ft_new_face h path (fromIntegral idx) ptrptr
            case ec of
              0 -> do fin <- mkDoneFace freeFace_
                      ptr <- peek ptrptr
                      p   <- newForeignPtr fin ptr
                      return (FTface p)
              _ -> fail ("newFace: failed to initialize, error " ++ show ec)



-- | Free the face, internally @FT_Done_Face@ will get called
-- to free the memory on the C side. 
doneFace :: FTface -> IO ()
doneFace (FTface h) = finalizeForeignPtr h


-- doneFace_ drops the return code as per doneLibrary_. 
freeFace_ :: FT_Face -> IO ()
freeFace_ p = ft_done_face p >> return ()

-- | @withForeignFace@ - internal function, shortahand for accessing the 
-- face pointer. 
withForeignFace :: FTface -> (FT_Face -> IO b) -> IO b 
withForeignFace (FTface fc) f = withForeignPtr fc $ \ h -> f h


--------------------------------------------------------------------------------

numFaces :: FTface -> IO Int
numFaces fc = withForeignFace fc fn where 
    fn = peekFace_num_faces >=> (return . fromIntegral)

faceIndex :: FTface -> IO Int
faceIndex fc = fromIntegral <$> withForeignFace fc peekFace_face_index
    
numGlyphs :: FTface -> IO Int
numGlyphs fc = fromIntegral <$> withForeignFace fc peekFace_num_glyphs
    
familyName :: FTface -> IO String
familyName fc = withForeignFace fc peekFace_family_name

styleName :: FTface -> IO String
styleName fc = withForeignFace fc peekFace_style_name

glyphSlotBitmap :: FTface -> IO Bitmap
glyphSlotBitmap fc = withForeignFace fc peekFace_bitmap

gsBitmapLeft :: FTface -> IO Int
gsBitmapLeft fc = 
    fromIntegral <$> withForeignFace fc peekFace_glyph_slot_bitmap_left

gsBitmapTop :: FTface -> IO Int
gsBitmapTop fc = 
    fromIntegral <$> withForeignFace fc peekFace_glyph_slot_bitmap_top


--------------------------------------------------------------------------------
--



selectSize :: FTface -> Int -> IO FTerror
selectSize fc strike_index = 
    withForeignFace fc $ \ h -> 
        ft_select_size h (fromIntegral strike_index)
        


setCharSize :: FTface -> F26d6 -> F26d6 -> Int -> Int -> IO FTerror
setCharSize fc cw ch hr vr = withForeignFace fc $ \h -> 
    ft_set_char_size h (getF26d6 cw)     (getF26d6 ch) 
                       (fromIntegral hr) (fromIntegral vr) 

setPixelSizes :: FTface -> Int -> Int -> IO FTerror
setPixelSizes fc pw ph = withForeignFace fc $ \h -> 
    ft_set_pixel_sizes h (fromIntegral pw) (fromIntegral ph) 

loadGlyph :: FTface -> Int -> [LoadFlag] -> IO FTerror 
loadGlyph fc gi flags = withForeignFace fc $ \h -> 
    ft_load_glyph h (fromIntegral gi) (combineLoadFlags flags) 

loadChar :: FTface -> Int -> [LoadFlag] -> IO FTerror 
loadChar fc ccode flags = withForeignFace fc $ \h ->  
    ft_load_char h (fromIntegral ccode) (combineLoadFlags flags) 


renderCurrentGlyph :: FTface -> RenderMode -> IO FTerror 
renderCurrentGlyph fc mode = withForeignFace fc $ \h -> do
    gly <- peekFace_glyph_slot h 
    ft_render_glyph gly (marshal mode)



combineLoadFlags :: [LoadFlag] -> FTint32 
combineLoadFlags = fromIntegral . foldr (\i a -> a .|. marshal i) 0 

postscriptName :: FTface -> IO String
postscriptName fc = withForeignFace fc $ \h -> do
    cstr <- ft_get_postscript_name h
    str  <- peekCString cstr
    return str
    


--------------------------------------------------------------------------------
-- FreeType library version number


type VersionNumber = (Int,Int,Int)

libraryVersion :: FTlibrary -> IO VersionNumber
libraryVersion (FTlibrary lib) =
  withForeignPtr lib $ \h -> 
    alloca $ \ptr_vmj -> do 
        alloca $ \ptr_vmn -> do 
            alloca $ \ptr_vph -> do                     
                ft_library_version h ptr_vmj ptr_vmn ptr_vph
                
                vmj <- peek ptr_vmj
                vmn <- peek ptr_vmn
                vph <- peek ptr_vph
                
                return (fromIntegral vmj, fromIntegral vmn, fromIntegral vph)


