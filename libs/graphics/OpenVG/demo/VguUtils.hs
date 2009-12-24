{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  VguUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  LGPL - this is a direct translation of Ivan Leben's code
--                (test.c) so its LGPL.
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Helper functions
--
--------------------------------------------------------------------------------


module VguUtils (
  testCreatePath,
  testDestoryPaths,
  
  testDrawString,
  testInit
) where

import Graphics.Rendering.OpenVG hiding ( 
        loadIdentity, lineWidth, scale, translate, matrixMode )
import qualified Graphics.Rendering.OpenVG as VG

import qualified Graphics.Rendering.OpenVG.Raw.VG.Core101 ( VGHandle )
import Graphics.UI.GLUT 

testCreatePath :: IO VGPath
testCreatePath = createPath VG.Float 1.0 0.0 0 0 [CapabilityAll]

testDestoryPaths :: [VGPath] -> IO ()
testDestoryPaths = mapM_ destroyPath


testDrawString :: GLfloat -> GLfloat -> String -> IO ()
testDrawString x y test_string = do
    
    blend       $= Enabled
    blendFunc   $= (SrcAlpha, OneMinusSrcAlpha)
    lineSmooth  $= Enabled
    multisample $= Disabled
    
    matrixMode  $= (Modelview 0) -- eh?
    preservingMatrix $ do
        loadIdentity
        translate (Vector3 (x+0.5) (y+0.5) 0)
        scale k k k
        lineWidth $= 1.0
        renderString Roman test_string
    
    lineSmooth  $= Disabled
  where
    k :: GLfloat
    k = 0.15
        
testInit :: Position -> Size -> String -> IO Window 
testInit xy sz title = do 
    (_progName, _args)    <- getArgsAndInitialize
    initialDisplayMode    $= [ RGBAMode, DoubleBuffered, WithAlphaComponent,
                               WithStencilBuffer, Multisampling ]
    initialWindowSize     $= sz
    initialWindowPosition $= xy
    win <- createWindow title
    -- ...
    okb <- VG.createContextSH $ sz                          
    if okb then putStrLn "testInit - okay" else error "testInit - failed"
    return win
   
    