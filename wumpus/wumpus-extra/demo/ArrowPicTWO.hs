

module ArrowPicTWO where

import Wumpus.Core
import Wumpus.Extra.Arrow
import Wumpus.Extra.Shape
import Wumpus.Extra.SVGColours

import Data.Monoid
import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    sequence_ [ demo01 ]


demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/arrowTWO.eps" picture1
    writeSVG_latin1 "./out/arrowTWO.svg" picture1

picture1 :: DPicture
picture1 = frameComposite $ temp_line


temp_line :: (Floating u, Real u) => Composite u
temp_line = ln `mappend` tip
  where
    ln  = simpleComposite $ ostroke () $ vertexPath [zeroPt , P2 100 100]
    tip = (arrowtip_drawing latexTip) 1.0 (pi/4) (P2 100 100)
     
