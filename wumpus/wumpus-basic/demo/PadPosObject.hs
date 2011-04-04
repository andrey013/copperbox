{-# OPTIONS -Wall #-}


module PadPosObject where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

import Control.Applicative 
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
    draw $ testDraw lPad   NN `at` (P2   0 0)
    draw $ testDraw rPad   NN `at` (P2 100 0)
    draw $ testDraw uPad   NN `at` (P2 200 0)
    draw $ testDraw dPad   NN `at` (P2 300 0)

    draw $ testDraw hPad   NN `at` (P2   0 100)
    draw $ testDraw vPad   NN `at` (P2 100 100)
    draw $ testDraw shrink NN `at` (P2 200 100)
    

lPad :: Trafo
lPad = padLeftPO 80

rPad :: Trafo
rPad = padRightPO 80

uPad :: Trafo
uPad = padUpPO 36

dPad :: Trafo
dPad = padDownPO 36

hPad :: Trafo
hPad = padHorizontalPO 90

vPad :: Trafo
vPad = padVerticalPO 48

shrink :: Trafo
shrink = padLeftPO 10

type Trafo = PosObject Double -> PosObject Double

testDraw :: Trafo -> RectAddress -> LocGraphic Double
testDraw trafo rpos = filledDisk 2 `oplus` bbobj
  where
    bbobj = ignoreAns $ illustrateBoundedLocGraphic $ 
              (lrgBox trafo `startAddr` rpos)




lrgBox :: Trafo -> BoundedLocRectGraphic Double
lrgBox trafo = makeBoundedLocRectGraphic $ trafo poBox 

poBox :: PosObject Double
poBox = makePosObject  mkOrtt mkRect
  where
    mkOrtt = (\ch dd -> Orientation 0 (5*ch) (-dd) ch) 
                <$> capHeight <*> descender

    mkRect = capHeight >>= \ch -> 
             descender >>= \dd -> 
             moveStart (move_down (abs dd)) 
                           $ borderedRectangle (5*ch) (ch + abs dd)


