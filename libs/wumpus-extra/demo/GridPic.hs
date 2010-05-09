
module GridPic where

import Wumpus.Core hiding ( blank )
import Wumpus.Doodle.Grid
import Wumpus.Extra



main :: IO ()
main = sequence_ [ demo01 ]



mgrid01 = do 
  
  gal_m           <- elem1 $ nil & blank         & node "Gal(M)" 
  
  (delta,gal_nm)  <- elem2 $ nil & node "/\\"    & node "Gal(N/M)"

  ee              <- elem1 $ nil & node "(E/E')" & blank

  return ()

demo01 = do 
    writeEPS_latin1 "./out/grid01.eps" pic1 
    writeSVG_latin1 "./out/grid01.svg" pic1 
  where
    pic1 :: Picture Double
    pic1   = nodePicture 100 50 3 elts no_pic
    elts   = snd $ grid mgrid01 
    no_pic = blankPicture (BBox zeroPt zeroPt)

