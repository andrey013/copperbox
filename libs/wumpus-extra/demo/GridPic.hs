
module GridPic where

import Wumpus.Core hiding ( blank )
import Wumpus.Extra.Matrix
import Wumpus.Extra



main :: IO ()
main = sequence_ [ demo01 ]

mgrid01 = do 
  
  gal_m           <- cell1 $ nil & blank         & node "Gal(M)" 
  
  (delta,gal_nm)  <- cell2 $ nil & node "/\\"    & node "Gal(N/M)"

  ee              <- cell1 $ nil & node "(E/E')" & blank

  connect gal_m  delta
  connect gal_m  gal_nm
  connect delta  gal_nm
  connect delta  ee
  connect ee     gal_nm

  return ()

demo01 = do 
    writeEPS_latin1 "./out/grid01.eps" pic1 
    writeSVG_latin1 "./out/grid01.svg" pic1 
  where
    pic1 :: Picture Double
    ((),st,w)   = grid mgrid01 
    pic1        = matrixPicture (V2 100 50) (st,w)

