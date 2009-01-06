{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HsGetString
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- The simplest example I can think of...
--

--------------------------------------------------------------------------------
-- Currently (on Windows) it seems to work best with `runhaskell` ...
-- shell> runhaskell.exe -i../src -lopenvg32 HsGetString.hs




module Main where

import Graphics.Rendering.OpenVG ( 
    createContextSH, destroyContextSH, StringID(..) , stringId )
import Graphics.UI.GLUT

main :: IO ()
main = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithAlphaComponent,
                            WithStencilBuffer, Multisampling ]
    initialWindowSize $= Size 100 100
    initialWindowPosition $= Position 0 0
    win <- createWindow progName
    
    okb <- createContextSH $ Size 100 100
    if okb then do { queryStr "OpenVG - Vendor "  Vendor
                   ; queryStr "OpenVG - Version " Version }
           else do { putStrLn "createContextSH failed - bad news!" }
    mainLoop
    destroyContextSH
    destroyWindow win
    putStrLn "Done."
  where
    queryStr :: String -> StringID -> IO ()
    queryStr msg sid = do 
        ans <- get $ stringId sid
        putStrLn $ msg ++ ans
             
    
    