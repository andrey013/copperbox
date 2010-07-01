
module GridPic where

import Wumpus.Core 
import Wumpus.Extra.Matrix
import Wumpus.Extra

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    sequence_ [ demo01 ]

mgrid01 :: GridM Width2 u ()
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
    pic1 = runMatrix (matrixProps (V2 100 50)) mgrid01

