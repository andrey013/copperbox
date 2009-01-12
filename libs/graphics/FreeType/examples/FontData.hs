{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  FontData
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Load a font and print some information about it...
--

--------------------------------------------------------------------------------
-- windows & cygwin with lbfreetype.dll in current directory 
-- > ghc --make -i../src -L. -lfreetype FTVersion.hs

-- For this example I've used Tempest.ttf available from:
-- http://www.fontfreak.com/charactermaps/t/Tempest.htm
-- and put in the directory \'./data\'

module Main where

import Graphics.Rendering.FreeType



main :: IO ()
main = do
    ft <- initFreeType
    fc <- newFace ft "data/Tempest.ttf" 0
    putStrLn $ "done load"
    
    n <- numFaces fc
    putStrLn $ show n ++ " faces in face"
    
    i <- faceIndex fc
    putStrLn $ "Face index " ++ show i
    
    j <- numGlyphs fc
    putStrLn $ show j ++ " glyphs"
    
    doneFace fc
    doneFreeType ft                 
    putStrLn "Done."
                

   