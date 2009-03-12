

-- :set -i../src

module ChalkboardBmp where


import Graphics.Chalkboard
import Graphics.BmpAdaptor


main :: IO ()
main = do 
    putStrLn "write"
    writeDemo
    putStrLn "read"
    readDemo

writeDemo :: IO ()
writeDemo = do
    writeBMP ("checker.bmp") (200,200) $ move (100,100) $ 
        fmap (\ x -> if x then green else white) 
      $ rotate 0.05 
      $ scale 50 
      $ checker 

readDemo :: IO ()    
readDemo = do
    (brd,(x,y)) <- readBMP "checker.bmp"
    putStrLn "read okay"
    writeBMP "checker2.bmp" (x,y) $ fmap (withDefault blue) brd 
    
       