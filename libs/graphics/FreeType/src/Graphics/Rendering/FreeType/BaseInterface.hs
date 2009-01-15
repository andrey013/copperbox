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
  
  -- * \'run a computation inside FreeType\'.
  withFreeType, 
  
  -- * newFace
  
  withNewFace,
  
  -- newFace,
  -- doneFace,
  
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
  
  Matrix(..), Vector(..),
  setTransform,
  
  RenderMode(..),
  renderCurrentGlyph, -- 
  
  postscriptName,
  
  Encoding(..),
  selectCharMap, 
  
  getCharIndex,
  

) where

import Graphics.Rendering.FreeType.FixedPrecision
import Graphics.Rendering.FreeType.Internals.CBaseInterface
import Graphics.Rendering.FreeType.Internals.CBasicDataTypes
import Graphics.Rendering.FreeType.Internals.Wrappers 
import Graphics.Rendering.FreeType.Utils ( nullForeignPtr )

import Control.Applicative ( (<$>) )
import Control.Exception ( bracket )
import Control.Monad
import Data.Bits ( (.|.) )
import Foreign.C.String ( withCString, peekCString )
import Foreign.ForeignPtr ( newForeignPtr, finalizeForeignPtr, 
    withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable ( peek )



-------------------------------------------------------------------------------- 

isNullFT_Library :: FT_library -> IO Bool
isNullFT_Library (FT_library lib) = nullForeignPtr lib

withFreeType :: a -> (FT_library -> IO a) -> IO a
withFreeType failureValue action = 
  bracket initFreeType doneFreeType 
          (\ftlib -> do { check_null <- isNullFT_Library ftlib 
                        ; if check_null
                             then do putStrLn "withFreeType: failed"
                                     return failureValue
                             else action ftlib })   
  
-- | Initialize a handle to the FreeType library, @FT_Init_FreeType@ 
-- allocates memory for the library on the C side
initFreeType :: IO FT_library
initFreeType =
  alloca $ \ptrptr -> do 
  ec <- ft_init_freetype ptrptr
  case ec of
    0 -> do fin <- mkDoneLibrary freeLibrary_
            ptr <- peek ptrptr
            p   <- newForeignPtr fin ptr
            return (FT_library p)
    _ -> fail ("initFreeType: failed to initialize, error " ++ show ec)


-- | Free the library handle, internally @FT_Done_FreeType@ will get called
-- to free the memory on the C side. 
doneFreeType :: FT_library -> IO ()
doneFreeType (FT_library h) = finalizeForeignPtr h

-- freeLibrary_ drops the return code from @ft_done_freetype@ so 
-- that the type signature is compatible with @FinalizerPtr@.
-- Note the argumnent is @FT_Library@ and not @FT_library@.
freeLibrary_ :: FT_library_ptr -> IO ()
freeLibrary_ p = ft_done_freetype p >> return ()


--------------------------------------------------------------------------------
-- New face
isNullFT_Face :: FT_face -> IO Bool
isNullFT_Face (FT_face lib) = nullForeignPtr lib


withNewFace :: FT_library -> FilePath -> Int -> a -> (FT_face -> IO a) -> IO a
withNewFace ft_lib path idx failureValue action = 
  bracket (newFace ft_lib path idx) doneFace 
          (\face -> do { check_null <- isNullFT_Face face 
                       ; if check_null
                            then do putStrLn "withFreeType: withNewFace"
                                    return failureValue
                            else action face }) 
                             
                               
                             
newFace :: FT_library -> FilePath -> Int -> IO FT_face
newFace (FT_library lib) path_s idx = 
    withForeignPtr lib $ \h ->
        withCString path_s $ \path ->  
            alloca $ \ptrptr -> do 
            ec <- ft_new_face h path (fromIntegral idx) ptrptr
            case ec of
              0 -> do fin <- mkDoneFace freeFace_
                      ptr <- peek ptrptr
                      p   <- newForeignPtr fin ptr
                      return (FT_face p)
              _ -> fail ("newFace: failed to initialize, error " ++ show ec)



-- | Free the face, internally @FT_Done_Face@ will get called
-- to free the memory on the C side. 
doneFace :: FT_face -> IO ()
doneFace (FT_face h) = finalizeForeignPtr h


-- doneFace_ drops the return code as per doneLibrary_. 
freeFace_ :: FT_face_ptr -> IO ()
freeFace_ p = ft_done_face p >> return ()





-- | @withForeignFace@ - internal function, shorthand for accessing the 
-- face pointer. 
withForeignFace :: FT_face -> (FT_face_ptr -> IO b) -> IO b 
withForeignFace (FT_face fc) f = withForeignPtr fc $ \ h -> f h


--------------------------------------------------------------------------------

numFaces :: FT_face -> IO Int
numFaces fc = withForeignFace fc fn where 
    fn = peekFace_num_faces >=> (return . fromIntegral)

faceIndex :: FT_face -> IO Int
faceIndex fc = fromIntegral <$> withForeignFace fc peekFace_face_index
    
numGlyphs :: FT_face -> IO Int
numGlyphs fc = fromIntegral <$> withForeignFace fc peekFace_num_glyphs
    
familyName :: FT_face -> IO String
familyName fc = withForeignFace fc peekFace_family_name

styleName :: FT_face -> IO String
styleName fc = withForeignFace fc peekFace_style_name

glyphSlotBitmap :: FT_face -> IO Bitmap
glyphSlotBitmap fc = withForeignFace fc peekFace_bitmap

gsBitmapLeft :: FT_face -> IO Int
gsBitmapLeft fc = 
    fromIntegral <$> withForeignFace fc peekFace_glyph_slot_bitmap_left

gsBitmapTop :: FT_face -> IO Int
gsBitmapTop fc = 
    fromIntegral <$> withForeignFace fc peekFace_glyph_slot_bitmap_top


--------------------------------------------------------------------------------
--



selectSize :: FT_face -> Int -> IO FT_error
selectSize fc strike_index = 
    withForeignFace fc $ \ h -> 
        ft_select_size h (fromIntegral strike_index)
        


setCharSize :: FT_face -> F26d6 -> F26d6 -> Int -> Int -> IO FT_error
setCharSize fc cw ch hr vr = withForeignFace fc $ \h -> 
    ft_set_char_size h (getF26d6 cw)     (getF26d6 ch) 
                       (fromIntegral hr) (fromIntegral vr) 

setPixelSizes :: FT_face -> Int -> Int -> IO FT_error
setPixelSizes fc pw ph = withForeignFace fc $ \h -> 
    ft_set_pixel_sizes h (fromIntegral pw) (fromIntegral ph) 

loadGlyph :: FT_face -> Int -> [LoadFlag] -> IO FT_error 
loadGlyph fc gi flags = withForeignFace fc $ \h -> 
    ft_load_glyph h (fromIntegral gi) (combineLoadFlags flags) 

loadChar :: FT_face -> Int -> [LoadFlag] -> IO FT_error 
loadChar fc ccode flags = withForeignFace fc $ \h ->  
    ft_load_char h (fromIntegral ccode) (combineLoadFlags flags) 

setTransform :: FT_face -> Matrix -> Vector -> IO ()
setTransform fc matrix delta = withForeignFace fc $ \h ->
    with matrix $ \ptr_matrix -> 
        with delta $ \ptr_delta -> ft_set_transform h ptr_matrix ptr_delta


renderCurrentGlyph :: FT_face -> RenderMode -> IO FT_error 
renderCurrentGlyph fc mode = withForeignFace fc $ \h -> do
    gly <- peekFace_glyph_slot h 
    ft_render_glyph gly (marshal mode)



combineLoadFlags :: [LoadFlag] -> FT_int32 
combineLoadFlags = fromIntegral . foldr (\i a -> a .|. marshal i) 0 

postscriptName :: FT_face -> IO String
postscriptName fc = withForeignFace fc $ \h -> do
    cstr <- ft_get_postscript_name h
    str  <- peekCString cstr
    return str


selectCharMap :: FT_face -> Encoding -> IO FT_error
selectCharMap fc enc = withForeignFace fc $ \h ->
    ft_select_charmap h (marshal enc)

getCharIndex :: FT_face -> Int -> IO Int
getCharIndex fc ccode = withForeignFace fc $ \h -> do
    idx <- ft_get_char_index h (fromIntegral ccode)
    return $ fromIntegral idx



