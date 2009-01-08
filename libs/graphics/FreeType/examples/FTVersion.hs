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

-- import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc


-- This is a straight translation of ft_demo.c so the code is very un-Haskell!
-- But it does test that the compile and link works.

main :: IO ()
main = do
    putStrLn $ "here"
    h <- malloc 
    err <- ft_Init_FreeType h
    putStrLn $ show err
    
    handle <- peek h
    ptr_vmj <- malloc
    ptr_vmn <- malloc
    ptr_vph <- malloc
    
    ft_Library_Version handle ptr_vmj ptr_vmn ptr_vph
    
    vmj <- peek ptr_vmj
    vmn <- peek ptr_vmn
    vph <- peek ptr_vph
    
    putStrLn $ show vmj ++ "." ++ show vmn ++ "." ++ show vph
     
    err2 <- ft_Done_FreeType handle             
    putStrLn $ show err2
    free ptr_vmj
    free ptr_vmn
    free ptr_vph
    free h
   