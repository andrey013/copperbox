{-# OPTIONS -Wall #-}

module UnitTest where

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.Monoid
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let out = runCtxPictureU std_attr pic01
    writeEPS "./out/unit_test.eps" out
    writeSVG "./out/unit_test.svg" out 
                        

std_attr :: DrawingContext
std_attr = standardContext 16


pic01 :: CtxPicture
pic01 = udrawTracing (0::Double) $ do

    drawl (P2  10   0) $ uconvF $ thing
    drawl (P2 110   0) $ uconvF $ thing2

    drawl (P2  10 100) $ uconvF $ thing
    drawl (P2 110 100) $ uconvF $ thing2

    drawl (P2  10 200) $ uconvF $ thing
    drawl (P2 110 200) $ uconvF $ thing2

    drawl (P2  10 300) $ uconvF $ thing
    drawl (P2 110 300) $ uconvF $ thing2


thing :: LocGraphic Em
thing = a `mappend` b
  where
    a = dcRectangle STROKE 2 (2::Em)
    b = moveStart (go_right (2::Em)) $ localize (scale_point_size 0.5) $
          dcRectangle FILL_STROKE 4 4


-- Pica is not contextual, so the localize should not matter...
--
thing2 :: LocGraphic Pica
thing2 = a `mappend` b
  where
    a = dcRectangle STROKE 2 2
    b = moveStart (go_right 2) $ localize (scale_point_size 0.5) $
          dcRectangle FILL_STROKE 4 4