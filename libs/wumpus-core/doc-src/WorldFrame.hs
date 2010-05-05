{-# OPTIONS -Wall #-}

module WorldFrame where

import Wumpus.Core
import Wumpus.Core.Colour ( black )


main :: IO ()
main = writeEPS_latin1 "WorldFrame.eps" world_frame

world_frame :: DPicture
world_frame = uniformScale 0.75 $ 
    frameMulti [ ogin, btm_right, top_left, top_right
               , x_axis, y_axis, line1
               ]
  where
    ogin      = makeLabelPrim "(0,0)"     (P2 0  0)
    btm_right = makeLabelPrim "(100,0)"   (P2 96 0)
    top_left  = makeLabelPrim "(0,100)"   (P2 0  114)
    top_right = makeLabelPrim "(100,100)" (P2 96 114)
    
    x_axis    = makeLinePrim 0.5 (P2 10 10) (P2 110 10)
    y_axis    = makeLinePrim 0.5 (P2 10 10) (P2 10 110)
    line1     = makeLinePrim 1.5 (P2 11 11) (P2 110 110)



makeLabelPrim :: String -> DPoint2 -> DPrimitive
makeLabelPrim text pt = textlabel attrs pt text
  where
    attrs     = (black, FontAttr "Helvetica" "Helvetica" SVG_REGULAR 10)

makeLinePrim :: Double -> DPoint2 -> DPoint2 -> DPrimitive
makeLinePrim lw a b = ostroke attrs $ path a [lineTo b]
  where
    attrs = (black, [LineWidth lw])