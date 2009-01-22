{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Example1
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Example1 from the FreeType tutorial
--

--------------------------------------------------------------------------------
-- windows & cygwin with lbfreetype.dll in current directory 
-- > ghc --make -i../src -L. -lfreetype FontData.hs

-- For this example I've used Tempest.ttf available from:
-- http://www.fontfreak.com/charactermaps/t/Tempest.htm
-- and put in the directory \'./data\'

module Main where

import Graphics.Rendering.FreeType
import Graphics.Rendering.FreeType.Utils
import Control.Monad

main :: IO ()
main = do
  withFreeType () $ \ft -> 
    withNewFace ft "data/Tempest.ttf" 0 () $ \fc -> do
        familyName fc       >>= putStrLn    
        styleName fc        >>= putStrLn 
        postscriptName fc   >>= putStrLn
        
        n <- numFaces fc
        putStrLn $ show n ++ " faces in face"
        
        i <- faceIndex fc
        putStrLn $ "Face index " ++ show i
        
        j <- numGlyphs fc
        putStrLn $ show j ++ " glyphs"
        
        setCharSize fc 0 (f26d6 (16::Int) (64::Int)) 150 150
        loadChar fc 36 [Render]  >>= putStrLn . show
        ec <- renderCurrentGlyph fc RenderNormal
        
        if ec==0 
          then do  
            putStrLn $ show ec
            withGlyphSlot fc $ \gs -> do 
                withBitmap gs printBitmap
                bitmapLeft gs >>= \x  -> putStrLn $ "glyphslot bitmap left " ++ show x 
                bitmapTop  gs >>= \x' -> putStrLn $ "glyphslot bitmap top "  ++ show x'
                
          else
            putStrLn $ "renderCurrentGlyph failed " ++ show ec
        
        withOutline ft 100 100 () $ \otl -> do
          cc <- contours otl
          putStrLn $ "n_contours " ++ show cc

  
        putStrLn "Done."


                
printBitmap :: Bitmap -> IO ()
printBitmap bmp = do 
    bs <- getBitmapBuffer bmp
    step bs
  where
    step [] = putStrLn $ ""
    step xs = let (a,b) = splitAt w xs in
              do { showLine a; step b }
    
    showLine = putStrLn . map f
      where f 255 = 'X'
            f _   = ' '

    w = getBitmapWidth bmp
    
         