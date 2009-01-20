{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  FTVersion
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- The simplest example I can think of...
--

--------------------------------------------------------------------------------
-- windows with lbfreetype.dll in current directory 
-- > ghc --make -i../src -L. -lfreetype FTVersion.hs


module Main where

import Graphics.Rendering.FreeType


main :: IO ()
main = do
    withFreeType () $ \ft -> do
        (mj,mn,ph) <- libraryVersion ft
        putStrLn $ show mj ++ "." ++ show mn ++ "." ++ show ph               
    putStrLn "Done."
                

   