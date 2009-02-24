{-# LANGUAGE RankNTypes #-}

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

module Graphics.ZBitmap.Convert (
  convertBmp,
  
  zbitmapToBmp24,
  
) where

import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.Utils ( physicalWidth, physicalSize, fold_lrdownM )
import Graphics.ZBitmap.ZBitmap


import Control.Monad.ST

import qualified Data.Array.MArray as MA
import Data.Array.ST ( runSTUArray, STUArray )
import Data.Array.Unboxed
import Data.Bits ( Bits(..) )
import qualified Data.ByteString as BS
import Data.Word



                         
--------------------------------------------------------------------------------
-- Bmp to internal bitmap conversion

-- Internal bitmap is a 32bit bitmap [red][green][blue][unused]
convertBmp :: BmpBitmap -> ZBitmap
convertBmp bmp = fn $ bits_per_pixel $ bmp_dibheader bmp where
    fn B1_Monochrome    = 
        mkBm $ runTranslate (stepMono ipw ih ipal ibs) ih iw oarr
    
    fn B4_Colour16      = 
        mkBm $ runTranslate (step4bit ipw ih ipal ibs) ih iw oarr
        
    fn B8_Colour256     = 
        mkBm $ runTranslate (step8bit ipw ih ipal ibs) ih iw oarr
        
    fn B24_TrueColour   = 
        mkBm $ runTranslate (stepTrueColour ipw ih 3 ibs) ih iw oarr
    
    fn B32_TrueColour   = 
        mkBm $ runTranslate (stepTrueColour ipw ih 4 ibs) ih iw oarr
        
    fn _                  = error $ "convertBmp - currently unhandled resolution"
    
    ih      = fromIntegral $ bmp_height $ bmp_dibheader bmp
    iw      = fromIntegral $ bmp_width  $ bmp_dibheader bmp
    ibpp    = bits_per_pixel $ bmp_dibheader bmp
    ibs     = bmp_body bmp
    ipw     = physicalWidth ibpp (iw)
    opw     = physicalWidth B32_TrueColour iw
    oarr    = newSurface ih opw
    ipal    = maybe fk palette_data $ bmp_opt_palette bmp
    fk      = error $ "convertBmp - missing palette for a format " 
                        ++ show ibpp
    mkBm a  = ZBitmap iw ih opw a

    
newSurface :: Int -> Int -> PixelSurface
newSurface row_count col_count = 
    runSTUArray $ MA.newArray ((0,0),(row_count - 1,col_count - 1)) 255
     

type LoopStep =  forall s. STUArray s (Int,Int) Word8 
              -> (Int,Int) 
              -> () 
              -> ST s () 

runTranslate :: LoopStep -> Int -> Int -> PixelSurface -> PixelSurface
runTranslate step row_count col_count uarr = 
    runSTUArray $ do
      marr <- MA.thaw uarr
      fold_lrdownM (step marr) row_count col_count () 
      return marr
      

stepMono :: Int -> Int -> PaletteData -> BmpDibImageData -> LoopStep
stepMono full_width height pal ibs oarr (r,c) st = 
    let row = height - 1 - r
        (x,xi)  = c `divMod` 8
        w8      = ibs `BS.index` (fromIntegral $ row * full_width + x)
        (PaletteColour rv gv bv) = atMsb pxLookup w8 (fromIntegral xi) 
    in do MA.writeArray oarr (r,c*4)   rv
          MA.writeArray oarr (r,c*4+1) gv
          MA.writeArray oarr (r,c*4+2) bv
          return st
  where         
    pxLookup b  = pal!(fromIntegral $ fromEnum b) 
    
    -- 0 msb - 7 lsb
    atMsb :: (Bits a) => (Bool -> b) -> a -> Int -> b  
    atMsb fun a i = fun $ a `testBit` j
      where j = (bitSize a) - 1 - i
      
step4bit :: Int -> Int -> PaletteData -> BmpDibImageData -> LoopStep
step4bit full_width height pal ibs oarr (r,c) st = 
    let row = height - 1 - r
        (x,xi)  = c `divMod` 2
        w8      = ibs `BS.index` (fromIntegral $ row * full_width + x)
        w4      = w8 `fourof` xi
        (PaletteColour rv gv bv) = pxLookup w4
    in do MA.writeArray oarr (r,c*4)   rv
          MA.writeArray oarr (r,c*4+1) gv
          MA.writeArray oarr (r,c*4+2) bv
          return st
  where         
    pxLookup b  = pal!(fromIntegral $ fromEnum b) 
    
    fourof :: Word8 -> Int -> Word8  
    -- first 4 bits
    fourof a 0 = (a .&. 0xf0) `shiftR` 4
    -- second four bits (the second arg will always be 0or1 so we use _ to match)
    fourof a _ = (a .&. 0x0f)
      
      
step8bit :: Int -> Int -> PaletteData -> BmpDibImageData -> LoopStep
step8bit full_width height pal ibs oarr (r,c) st = 
    let row = height - 1 - r
        w8      = ibs `BS.index` (fromIntegral $ row * full_width + c)
        (PaletteColour rv gv bv) = pxLookup w8 
    in do MA.writeArray oarr (r,c*4)   rv
          MA.writeArray oarr (r,c*4+1) gv
          MA.writeArray oarr (r,c*4+2) bv
          return st
  where         
    pxLookup b  = pal!(fromIntegral $ fromEnum b) 
          
stepTrueColour :: Int -> Int -> Int -> BmpDibImageData -> LoopStep
stepTrueColour full_width height increment ibs oarr (r,c) st = 
    let row = height - 1 - r
        base = fromIntegral $ row * full_width + c * increment
        bl = ibs `BS.index` base
        gn = ibs `BS.index` (base+1)
        rd = ibs `BS.index` (base+2)  
    in do MA.writeArray oarr (r,c*4)   rd
          MA.writeArray oarr (r,c*4+1) gn
          MA.writeArray oarr (r,c*4+2) bl
          return st


  

--------------------------------------------------------------------------------
--  Convert the internal bitmap to a 24bit bmp bitmap

zbitmapToBmp24 :: ZBitmap -> BmpBitmap
zbitmapToBmp24 bmp@(ZBitmap w h _ _) = BmpBitmap hdr dibh Nothing img
  where
    w', h' :: Word32
    (w',h') = (fromIntegral w,fromIntegral h) 
    ps      = fromIntegral $ physicalSize B24_TrueColour w h
    hdr     = makeBmpHeaderShort 0 ps
    dibh    = makeBmpDibHeaderShort w' h' B24_TrueColour ps
    img     = makeImageData bmp

-- drat! - where is the library function to go from @UArray of Word8@
-- to a @Word8 ByteString@ ? 
slowMarshal :: UArray Int Word8 -> BS.ByteString
slowMarshal = BS.pack . elems
    
makeImageData :: ZBitmap -> BmpDibImageData
makeImageData (ZBitmap w h _ bmp) = 
    slowMarshal $ makeLinear h w rwidth bmp arr 
  where
    ps      = physicalSize  B24_TrueColour w h
    rwidth  = physicalWidth B24_TrueColour w
    arr     = newOutputArray ps

makeLinear :: Int -> Int -> Int -> PixelSurface 
                  -> UArray Int Word8 -> UArray Int Word8
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

                         
newOutputArray :: ByteCount -> UArray Int Word8
newOutputArray sz = listArray (0,sz) (replicate sz 0)


       