

-- :set -i../extra:../src

module DemoCB where


import Graphics.Chalkboard
import Graphics.ZBitmap
import ChalkboardAdaptor


main = do
  writeBMP ("checker.bmp") (200,200) $ move (100,100) $ 
      fmap (\ x -> if x then green else white) 
    $ rotate 0.05 
    $ scale 50 
    $ checker 
    
readdemo = do
    (brd,(x,y)) <- readBMP "checker.bmp"
    writeBMP "checker2.bmp" (x,y) $ fmap (withDefault blue) brd 
    
       