
module GridPic where

import Wumpus.Core
import Wumpus.Extra



main :: IO ()
main = sequence_ [ demo01 ]



mgrid01 = do 
  nextcol 
  gal_m   <- node "Gal(M)" 
  row 
  delta   <- node "delta" 
  gal_nm  <- node "Gal(N/M)"
  row 
  ee      <- node "(E/E')"

  return ()

demo01 = do 
    writeEPS "./out/grid01.eps" (Just ("Courier", 12)) pic1 
    writeSVG "./out/grid01.svg" pic1 
  where
   
    pic1   = nodePicture 100 50 3 elts no_pic
    elts   = snd $ grid mgrid01 
    no_pic = blankPicture (BBox zeroPt zeroPt)


