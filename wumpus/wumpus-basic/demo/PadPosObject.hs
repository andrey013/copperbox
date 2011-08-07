{-# OPTIONS -Wall #-}


module PadPosObject where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour

import Control.Applicative 
import Data.Monoid
import System.Directory



main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx drawing01
    writeEPS "./out/pad_pos_object01.eps" pic1
    writeSVG "./out/pad_pos_object01.svg" pic1


std_ctx :: DrawingContext
std_ctx = standardContext 24


drawing01 :: CtxPicture
drawing01 = drawTracing $ localize (fill_colour red) $ mf 


mf :: TraceDrawing Double ()
mf = do

    draw $ testDraw id     CENTER `at` (P2   0 0)
    draw $ testDraw lPad   CENTER `at` (P2 100 0)
    draw $ testDraw rPad   CENTER `at` (P2 200 0)
    draw $ testDraw uPad   CENTER `at` (P2 300 0)
    draw $ testDraw dPad   CENTER `at` (P2 400 0)

    draw $ testDraw hPad   CENTER `at` (P2   0 100)
    draw $ testDraw vPad   CENTER `at` (P2 100 100)
    draw $ testDraw shrink CENTER `at` (P2 200 100)
    
--
-- NOTE - The output is wrong and @mapOrientation@ looks 
-- troublesome. The best thing to do is re-implement padding and
-- filling and remove mapOrientation.
--

lPad :: Trafo
lPad = mapOrientation (fillXMinor 60)

rPad :: Trafo
rPad = mapOrientation (fillXMajor 60)

uPad :: Trafo
uPad = mapOrientation (fillYMajor 30)

dPad :: Trafo
dPad = mapOrientation (fillYMinor 40)

hPad :: Trafo
hPad = mapOrientation (fillHEven 60)

vPad :: Trafo
vPad = mapOrientation (fillVEven 40)

shrink :: Trafo
shrink = mapOrientation (fillYMinor (-10))

type Trafo = PosGraphic Double -> PosGraphic Double

testDraw :: Trafo -> RectAddress -> LocGraphic Double
testDraw trafo rpos = dcDisk FILL 2 `mappend` bbobj
  where
    bbobj = ignoreAns $ illustrateBoundedLocGraphic $ lrgBox trafo rpos




lrgBox :: Trafo -> (RectAddress -> BoundedLocGraphic Double)
lrgBox trafo = \addr -> runPosObjectBBox (trafo poBox) addr

poBox :: PosGraphic Double
poBox = makePosObject mkOrtt mkRect
  where
    mkOrtt = pure (Orientation 20 20 10 10) 
    mkRect = moveStart (V2 (-20) (-10)) $ dcRectangle STROKE 40 20 

    -- dcRectangle is start point bottom left
