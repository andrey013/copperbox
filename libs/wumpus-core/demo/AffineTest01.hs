

module PrimAffine where

import Wumpus.Core
import Wumpus.Core.Colour ( black, red )

main :: IO ()
main = do { putStrLn "Rotated text..."
          ; writeEPS_latin1 "./out/AffineTest01a.eps" rot_text
          ; writeSVG_latin1 "./out/AffineTest01a.svg" rot_text 
          }

light_blue :: DRGB
light_blue = iRGB3 176 224 231

label1 :: DRGB -> DPrimitive
label1 rgb = textlabel rgb "Wumpus!" zeroPt

pic1   :: DPicture
pic1   = illustrateBounds light_blue $ rotate45 $ frame $ label1 black

pic2   :: DPicture
pic2   = illustrateBoundsPrim light_blue $ rot_prim
  where
   rot_prim :: DPrimitive
   rot_prim = rotate45 $ label1 red

rot_text :: DPicture
rot_text = pic1 `picBeside` pic2