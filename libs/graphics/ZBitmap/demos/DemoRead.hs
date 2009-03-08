{-# OPTIONS -Wall #-}

module Main where


import Graphics.ZBitmap
import Text.PrettyPrint.HughesPJ ( render )

import Graphics.ZBitmap.InternalBitmap 

import Data.Array

dummy :: Array (Int,Int) Char
dummy = cstyle2Darray 4 2 "aaaabbbb"


main :: IO ()
main = run24bit
    
runMono :: IO ()
runMono = 
    runAction ("../images/mono/sizes/monow33H10.bmp", "./out/mono1.bmp", True)

run4bit :: IO ()    
run4bit = 
    runAction ("../images/4bit/red16colour.bmp", "./out/red16colour.bmp", True)
    
run8bit :: IO ()    
run8bit = 
    runAction ("../images/8bit/picture256.bmp", "./out/picture256.bmp", True)
    

run24bit :: IO ()
run24bit = 
    runAction ("../images/24bit/luke.bmp", "./out/luke_2.bmp", False)
    
    
runAction :: (FilePath, FilePath, Bool) -> IO ()
runAction (infile,outfile,show_palette) = do
    a <- readBmp infile
    showBmp a
    let ub = uniBitmap a
    print ub
    (putStrLn . showAsciiPicture . makeAsciiPicture) ub
    
    
{-
    if show_palette 
      then maybe fk sk (optPalette a)
      else putStrLn "No palette"
    let a' = zbitmapToBmp24 b
    writeBmp outfile a'
-}
  where
    fk :: IO ()
    fk = putStrLn $ "No palette spec"
    
    sk :: Palette -> IO ()
    sk pal = putStrLn $ render $ ppPalette pal  
      
    showBmp :: BmpBitmap -> IO ()
    showBmp bmp = do 
        putStrLn $ render $ ppBmpBitmap bmp
        putStr "\n"
    


