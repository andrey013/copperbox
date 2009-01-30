{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.Convert
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Convert between bitmap representations
--
--------------------------------------------------------------------------------

module ZBitmap.Convert (
  palette,
  convertBmp,
  
  bitmapToBmp24,
  
) where

import ZBitmap.Datatypes
import ZBitmap.Utils ( physicalWidth, physicalSize, fold_lrdownM )

import Data.Array.IArray ( (!), elems )
import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTArray, runSTUArray )
import Data.Array.Unboxed ( UArray )
import Data.Bits ( Bits(..) )
import qualified Data.ByteString as BS
import Data.Word

import Prelude hiding ( (^) )
import qualified Prelude as Pre

-- Avoid warnings due to Integral-Integer defaulting 
infixr 8 ^
(^) :: Integral a => a -> a -> a
(^) = (Pre.^)


--------------------------------------------------------------------------------
-- Palette manipulation

-- | Make a palette from the Bmp byte-string 
palette :: BmpBitsPerPixel -> BmpPaletteSpec -> Palette
palette bpp bs = 
    maybe (fk bpp) (\(sz,blen) -> makePalette sz blen bs) $ 
              checkPaletteSize bpp bs 
  where
    fk B1_Monochrome = error $ msg ++ show (4*(2^1)::Int) ++ " for a mono bitmap"
    fk B4_Colour16   = error $ msg ++ show (4*(2^4)::Int) ++ " for a 16 colour bitmap" 
    fk B8_Colour256  = error $ msg ++ show (4*(2^8)::Int) ++ " for a 256 colour bitmap"
    fk _             = error $ "No palette spec for this resolution."
    
    msg = "Error: palette - unrecognized palette size, data length is " 
          ++ show (BS.length bs) ++ "\nbut it should be " 


checkPaletteSize :: BmpBitsPerPixel -> BS.ByteString -> Maybe (Word32,Int)
checkPaletteSize bpp bs = fn bpp (BS.length bs) where
    fn B1_Monochrome sz | sz == 4*(2^1)  = Just (2^1, sz) 
    fn B4_Colour16   sz | sz == 4*(2^4)  = Just (2^4, sz)
    fn B8_Colour256  sz | sz == 4*(2^8)  = Just (2^8, sz)
    fn _             _                    = Nothing

    
makePalette :: Word32 -> Int -> BS.ByteString -> Palette
makePalette sz bs_len bs = (\p -> Palette sz p) $ runSTArray $ do
    pal <-  MA.newArray (0, fromIntegral sz - 1) (RgbColour 0 0 0)
    step pal 0 0
    return pal
  where
    step pal ix ox 
        | ix >= bs_len   = return ()
        -- Note colours are in reverse order [blue][green][red][none]
        -- in the BMP palette.                          
        | otherwise      = let rd = bs `BS.index` (ix+2)
                               gn = bs `BS.index` (ix+1)
                               bl = bs `BS.index` ix
                           in do MA.writeArray pal ox (RgbColour rd gn bl)
                                 step pal (ix+4) (ox+1)

                                      
                                       
--------------------------------------------------------------------------------
-- Bmp to internal bitmap conversion

-- Internal bitmap is a 32bit bitmap [red][green][blue][unused]
convertBmp :: BmpBitmap -> Bitmap
convertBmp bmp = fn $ bitsPerPixelBmp bmp where
    fn B1_Monochrome      = mkBm $ translateMono ih iw ipw ipal ibs oarr
    fn B24_TrueColour24   = mkBm $ translate24bit ih iw ipw ibs oarr
    fn _                  = error $ "convertBmp - currently unhandled resolution"

    (ih,iw) = (heightBmp bmp, widthBmp bmp)
    ibpp    = bitsPerPixelBmp bmp
    ibs     = imageDataBmp bmp
    ipw     = physicalWidth ibpp (iw)
    opw     = physicalWidth B32_TrueColour32 iw
    oarr    = newSurface ih opw
    ipal    = maybe fk (palette ibpp) (optPaletteSpecBmp bmp)
    fk      = error $ "convertBmp - missing palette for a format " 
                        ++ show ibpp
    mkBm a  = Bitmap iw ih opw a

    
newSurface :: Word32 -> Word32 -> PixelSurface
newSurface row_count col_count = 
    runSTUArray $ MA.newArray ((0,0),(row_count - 1,col_count - 1)) 255
    
translate24bit :: 
    Word32 -> Word32 -> Word32 -> BmpDibImageData -> PixelSurface -> PixelSurface
translate24bit row_count col_count full_width bs uarr = runSTUArray $ do
    bmp <- MA.thaw uarr
    fold_lrdownM (f bmp) row_count col_count () 
    return bmp
  where
    f bmp (r,c) _ = let row = row_count - 1 - r
                        base = fromIntegral $ row * full_width + c*3
                        bl = bs `BS.index` base
                        gn = bs `BS.index` (base+1)
                        rd = bs `BS.index` (base+2)  
                    in do MA.writeArray bmp (r,c*4)   rd
                          MA.writeArray bmp (r,c*4+1) gn
                          MA.writeArray bmp (r,c*4+2) bl
                       

    
translateMono :: 
    Word32 -> Word32 -> Word32 -> Palette -> 
        BmpDibImageData -> PixelSurface -> PixelSurface
translateMono row_count col_count full_width (Palette _ pal) bs uarr = 
    runSTUArray $ do
        bmp <- MA.thaw uarr
        fold_lrdownM (f bmp) row_count col_count () 
        return bmp
  where
    f bmp (r,c) _ = let row = row_count - 1 - r
                        (x,xi)  = c `divMod` 8
                        w8      = bs `BS.index` (fromIntegral $ row * full_width + x)
                        (RgbColour rd gn bl) = atMsb pxLookup w8 (fromIntegral xi) 
                    in do MA.writeArray bmp (r,c*4)   rd
                          MA.writeArray bmp (r,c*4+1) gn
                          MA.writeArray bmp (r,c*4+2) bl
                          
    pxLookup b  = pal!(fromIntegral $ fromEnum b) 
    
    -- 0 msb - 7 lsb
    atMsb :: (Bits a) => (Bool -> b) -> a -> Int -> b  
    atMsb fun a i = fun $ a `testBit` j
      where j = (bitSize a) - 1 - i
  



  
  {-
monoTo24bit :: BmpBitmap -> Bitmap
monoTo24bit bm = 
    (\bmp -> Bitmap w h pw32 bmp) 
          $ translateMono h w pw pal (imageDataBmp bm) 
          $ newSurface h pw32
  where
    w     = widthBmp bm
    h     = heightBmp bm
    bpp   = bitsPerPixelBmp bm
    pw    = physicalWidth bpp w
    pw32  = physicalWidth B32_TrueColour32 w
    pal   = palette bpp (fromJust $ optPaletteSpecBmp bm)
-}

--------------------------------------------------------------------------------
--   





bitmapToBmp24 :: Bitmap -> BmpBitmap
bitmapToBmp24 bmp@(Bitmap w h _ _) = makeBmpBitmap hdr dibh Nothing img
  where
    ps    = physicalSize B24_TrueColour24 w h
    hdr   = makeBmpHeaderShort 0 ps
    dibh  = makeBmpDibHeaderShort w h B24_TrueColour24 ps
    img   = makeImageData bmp

-- drat! - where is the library function to go from @UArray of Word8@
-- to a @Word8 ByteString@ ? 
slowMarshal :: UArray Word32 Word8 -> BS.ByteString
slowMarshal = BS.pack . elems
    
makeImageData :: Bitmap -> BmpDibImageData
makeImageData (Bitmap w h _ bmp) = 
    slowMarshal $ makeLinear h w rwidth bmp arr 
  where
    ps      = physicalSize B24_TrueColour24 w h
    rwidth  = physicalWidth B24_TrueColour24 w
    arr     = newOutputArray ps

makeLinear :: Word32 -> Word32 -> Word32 -> PixelSurface 
                  -> UArray Word32 Word8 -> UArray Word32 Word8
makeLinear row_count col_count row_width bmp marr = runSTUArray $ do
    zz <- MA.thaw marr
    fold_lrdownM (f zz) row_count col_count () 
    return zz
  where
    f line (r,c) _ = let lrow = row_count - 1 - r
                         rd   = bmp!(r,c*4)
                         gn   = bmp!(r,c*4+1)
                         bl   = bmp!(r,c*4+2)
                     in do MA.writeArray line (lrow * row_width + c * 3)     bl
                           MA.writeArray line (lrow * row_width + c * 3 + 1) gn
                           MA.writeArray line (lrow * row_width + c * 3 + 2) rd

                         
newOutputArray :: ByteCount -> UArray Word32 Word8
newOutputArray sz = runSTUArray $ MA.newArray (0,sz) 0


       