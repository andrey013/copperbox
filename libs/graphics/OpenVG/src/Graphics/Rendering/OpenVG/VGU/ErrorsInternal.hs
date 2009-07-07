{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenVG.VG.Errors
-- Copyright   :  (c) Stephen Tetley 2008, 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- This module corresponds to section 4.1 (Errors) 
-- of the OpenVG 1.0.1 specs.
--
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenVG.VGU.ErrorsInternal (
   Error(..), ErrorCategory(..), getErrors,
   recordErrorCode, recordBadHandle, recordIllegalArgument, recordOutOfMemory
) where

import Graphics.Rendering.OpenVG.VG.BasicTypes ( VGenum ) 
import Graphics.Rendering.OpenVG.VG.CFunDecls ( vgGetError )
import Graphics.Rendering.OpenVG.VG.Constants (
      vg_NO_ERROR, 
      vg_BAD_HANDLE_ERROR, 
      vg_ILLEGAL_ARGUMENT_ERROR,
      vg_OUT_OF_MEMORY_ERROR, 
      vg_PATH_CAPABILITY_ERROR,
      vg_UNSUPPORTED_IMAGE_FORMAT_ERROR, 
      vg_UNSUPPORTED_PATH_FORMAT_ERROR,
      vg_IMAGE_IN_USE_ERROR,
      vg_NO_CONTEXT_ERROR ) 
import Graphics.Rendering.OpenVG.VGU.CInternals

import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------
-- Follow the convention of the OpenGL binding, both VG and VGU errors are 
-- handled in this module

--------------------------------------------------------------------------------
-- | VG errors.

data VG_ErrorCode =
     VG_NoError
   | VG_BadHandle
   | VG_IllegalArgument
   | VG_OutOfMemory
   | VG_PathCapability
   | VG_UnsupportedImageFormat
   | VG_UnsupportedPathFormat
   | VG_ImageInUse
   | VG_NoContextError
   deriving ( Eq, Ord, Show )
   

vg_marshalErrorCode :: VG_ErrorCode -> VGenum
vg_marshalErrorCode x = case x of
    VG_NoError  -> vg_NO_ERROR
    VG_BadHandle -> vg_BAD_HANDLE_ERROR
    VG_IllegalArgument -> vg_ILLEGAL_ARGUMENT_ERROR
    VG_OutOfMemory -> vg_OUT_OF_MEMORY_ERROR
    VG_PathCapability -> vg_PATH_CAPABILITY_ERROR
    VG_UnsupportedImageFormat -> vg_UNSUPPORTED_IMAGE_FORMAT_ERROR
    VG_UnsupportedPathFormat -> vg_UNSUPPORTED_PATH_FORMAT_ERROR
    VG_ImageInUse -> vg_IMAGE_IN_USE_ERROR
    VG_NoContextError -> vg_NO_CONTEXT_ERROR
   
--------------------------------------------------------------------------------
-- | VGU errors.
       
data VGU_ErrorCode = 
     VGU_NoError
   | VGU_BadHandle
   | VGU_IllegalArgument
   | VGU_OutOfMemory
   | VGU_PathCapability
   | VGU_BadWarp
   deriving ( Eq, Ord, Show )

vgu_marshalErrorCode :: VGU_ErrorCode -> VGenum
vgu_marshalErrorCode x = case x of
    VGU_NoError -> vgu_NO_ERROR
    VGU_BadHandle -> vgu_BAD_HANDLE_ERROR
    VGU_IllegalArgument -> vgu_ILLEGAL_ARGUMENT_ERROR
    VGU_OutOfMemory -> vgu_OUT_OF_MEMORY_ERROR
    VGU_PathCapability -> vgu_PATH_CAPABILITY_ERROR
    VGU_BadWarp -> vgu_BAD_WARP_ERROR


--------------------------------------------------------------------------------

-- | OpenVG errors have no error string - so this is slight different from 
-- HsOpenGL.

data Error = Error ErrorCategory
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Ammalganted error categories

data ErrorCategory =
     BadHandle
   | IllegalArgument
   | OutOfMemory
   | PathCapability
   | UnsupportedImageFormat
   | UnsupportedPathFormat
   | ImageInUse
   | NoContextError
   | BadWarp
   deriving ( Eq, Ord, Show )

unmarshalErrorCategory :: VGenum -> ErrorCategory
unmarshalErrorCategory c
   | isBadHandle c                = BadHandle
   | isIllegalArgument c          = IllegalArgument
   | isOutOfMemory c              = OutOfMemory
   | isPathCapability c           = PathCapability
   | isUnsupportedImageFormat c   = UnsupportedImageFormat
   | isUnsupportedPathFormat c    = UnsupportedPathFormat
   | isImageInUse c               = ImageInUse
   | isNoContextError c           = NoContextError
   | isBadWarp c                  = BadWarp
   | otherwise = error "unmarshalErrorCategory"
   
   
isBadHandle :: VGenum -> Bool
isBadHandle c =
   c == vg_marshalErrorCode  VG_BadHandle ||
   c == vgu_marshalErrorCode VGU_BadHandle

isIllegalArgument :: VGenum -> Bool
isIllegalArgument c =
   c == vg_marshalErrorCode  VG_IllegalArgument ||
   c == vgu_marshalErrorCode VGU_IllegalArgument

isOutOfMemory :: VGenum -> Bool
isOutOfMemory c =
   c == vg_marshalErrorCode  VG_OutOfMemory ||
   c == vgu_marshalErrorCode VGU_OutOfMemory

isPathCapability :: VGenum -> Bool
isPathCapability c =
   c == vg_marshalErrorCode  VG_PathCapability ||
   c == vgu_marshalErrorCode VGU_PathCapability
      
isUnsupportedImageFormat :: VGenum -> Bool
isUnsupportedImageFormat c =
   c == vg_marshalErrorCode  VG_UnsupportedImageFormat
      
isUnsupportedPathFormat :: VGenum -> Bool
isUnsupportedPathFormat c =
   c == vg_marshalErrorCode  VG_UnsupportedPathFormat
        
isImageInUse :: VGenum -> Bool
isImageInUse c =
   c == vg_marshalErrorCode  VG_ImageInUse
   
isNoContextError :: VGenum -> Bool
isNoContextError c =
   c == vg_marshalErrorCode  VG_NoContextError
   
isBadWarp :: VGenum -> Bool
isBadWarp c =
   c == vgu_marshalErrorCode  VGU_BadWarp

--------------------------------------------------------------------------------

-- Unlike OpenGL, there is no error description string to inspect

makeError :: VGenum -> IO Error
makeError e = do
   let category = unmarshalErrorCategory e
   return $ Error category
   
      
--------------------------------------------------------------------------------

-- Verbatim from GLU ErrorsInternal just type names changed.

{-# NOINLINE theRecordedErrors #-}
theRecordedErrors :: IORef ([VGenum],Bool)
theRecordedErrors = unsafePerformIO (newIORef ([], True))

getRecordedErrors :: IO ([VGenum],Bool)
getRecordedErrors =  readIORef theRecordedErrors

setRecordedErrors :: ([VGenum],Bool) -> IO ()
setRecordedErrors = writeIORef theRecordedErrors


--------------------------------------------------------------------------------

getVGErrors :: IO [VGenum]
getVGErrors = getVGErrorsAux []
   where getVGErrorsAux acc = do
            errorCode <- vgGetError
            if isError errorCode
               then getVGErrorsAux (errorCode : acc)
               else return $ reverse acc

isError :: VGenum -> Bool
isError = (/= vg_marshalErrorCode VG_NoError)


--------------------------------------------------------------------------------

getErrors :: IO [Error]
getErrors = do
   es <- getErrorCodesAux (const ([], True))
   mapM makeError es

recordErrorCode :: VGenum -> IO ()
recordErrorCode e = do
   getErrorCodesAux (\es -> (if null es then [e] else [], False))
   return ()

recordBadHandle :: IO ()
recordBadHandle = recordErrorCode (vg_marshalErrorCode VG_BadHandle)

recordIllegalArgument :: IO ()
recordIllegalArgument = recordErrorCode (vg_marshalErrorCode VG_IllegalArgument)

recordOutOfMemory :: IO ()
recordOutOfMemory = recordErrorCode (vg_marshalErrorCode VG_OutOfMemory)

-- ToDo: Make this thread-safe
getErrorCodesAux :: ([VGenum] -> ([VGenum],Bool)) -> IO [VGenum]
getErrorCodesAux f = do
   (recordedErrors, useVGErrors) <- getRecordedErrors
   vgErrors <- getVGErrors
   let es = if useVGErrors then recordedErrors ++ vgErrors else recordedErrors
   setRecordedErrors (f es)
   return es
