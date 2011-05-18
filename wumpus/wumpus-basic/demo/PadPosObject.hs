{-# OPTIONS -Wall #-}


module PadPosObject where

import Wumpus.Basic.Kernel

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( red )

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
    draw $ testDraw lPad   NN `at` (P2   0 0)
    draw $ testDraw rPad   NN `at` (P2 100 0)
    draw $ testDraw uPad   NN `at` (P2 200 0)
    draw $ testDraw dPad   NN `at` (P2 300 0)

    draw $ testDraw hPad   NN `at` (P2   0 100)
    draw $ testDraw vPad   NN `at` (P2 100 100)
    draw $ testDraw shrink NN `at` (P2 200 100)
    

lPad :: Trafo
lPad = mapOrientation (padXMinor 80)

rPad :: Trafo
rPad = mapOrientation (padXMajor 80)

uPad :: Trafo
uPad = mapOrientation (padYMajor 36)

dPad :: Trafo
dPad = mapOrientation (padYMinor 36)

hPad :: Trafo
hPad = mapOrientation (padHEven 90)

vPad :: Trafo
vPad = mapOrientation (padVEven 48)

shrink :: Trafo
shrink = mapOrientation (padYMinor (-10))

type Trafo = PosObject Double -> PosObject Double

testDraw :: Trafo -> RectAddress -> LocGraphic Double
testDraw trafo rpos = dcDisk FILL 2 `mappend` bbobj
  where
    bbobj = ignoreAns $ illustrateBoundedLocGraphic $ 
              lrgBox trafo rpos




lrgBox :: Trafo -> (RectAddress -> BoundedLocGraphic Double)
lrgBox trafo = \raddr -> runPosObject raddr $ trafo poBox 

poBox :: PosObject Double
poBox = makePosObject  mkOrtt mkRect
  where
    mkOrtt = (\ch dd -> Orientation 0 (5*ch) (-dd) ch) 
                <$> capHeight <*> descender

    mkRect = capHeight >>= \ch -> 
             descender >>= \dd -> 
             moveStart (go_down (abs dd)) 
                           $ dcRectangle FILL_STROKE (5*ch) (ch + abs dd)


