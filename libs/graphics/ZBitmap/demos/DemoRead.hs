{-# OPTIONS -Wall #-}

module Main where


import ZBitmap
import Text.PrettyPrint.HughesPJ ( render )

main :: IO ()
main = do
  a <- readBmp "../images/24bit/luke.bmp"
  -- a <- readBmp "../images/24bit/letterA.bmp"
  -- a <- readBmp "../images/mono/letterA.bmp"
  showBmp a
  let b = dibToBitmap a
  putStrLn $ showAsciiPicture $ makeAsciiPicture b
  
  
showBmp :: BMPfile -> IO ()
showBmp bmp@(BMPfile h dh _ body) = do 
    putStrLn $ render $ ppBMPheader h
    putStrLn $ render $ ppV3DibHeader dh
    putStr "\n"
 
{-     
    let pic = makeAsciiPicture (_bmp_height dh) (_bmp_width dh) (getImg body)
    putStrLn $ showAsciiPicture pic
-}    


