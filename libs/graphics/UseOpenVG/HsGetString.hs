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
-- Currently (on Windows) it only seems to work via ghci (??)
-- ~> ghcii.sh -i../OpenVG -lopenvg32
-- ghci> :load HsGetString.hs
-- ghci> main

-- This opens a glutWindow - closing it kills ghci.
-- (But we need a glutWindow to create a vgContext inside).


module Main where

import Graphics.Rendering.OpenVG
import Graphics.UI.GLUT

main :: IO ()
main = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode, DoubleBuffered, WithAlphaComponent,
                            WithStencilBuffer, Multisampling ]
    initialWindowSize $= Size 100 100
    initialWindowPosition $= Position 0 0
    createWindow progName
    
    okb <- createContextSH 10 10
    if okb then do { queryStr "OpenVG - Vendor " Vendor
                   ; queryStr "OpenVG - Version " Version }
           else do { putStrLn "createContextSH failed - bad news!" } 
    destroyContextSH
    putStrLn "Done."
  where
    queryStr :: String -> StringID -> IO ()
    queryStr msg sid = do 
        ans <- getString sid
        case ans of 
          Left err -> putStrLn $ "error - getString " ++ show sid 
          Right s -> putStrLn $ msg ++ s
             
    
    