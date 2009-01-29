{-# OPTIONS -Wall #-}

module Main where


import ZBitmap
import Text.PrettyPrint.HughesPJ ( render )

main :: IO ()
main = do
    a <- readBmp "../images/24bit/luke.bmp"
    -- a <- readBmp "../images/24bit/letterA.bmp"
    -- a <- readBmp "../images/8bit/picture256.bmp"
    -- a <- readBmp "../images/mono/letterA.bmp"
    showBmp a
    let b = dibToBitmap a
    putStrLn $ showAsciiPicture $ makeAsciiPicture b
    maybe fk (sk a) (optPaletteSpecBmp a)
  where
    fk = putStrLn $ "No palette spec"
    sk a ps = let pal = palette (bitsPerPixelBmp a) ps 
              in putStrLn $ render $ ppPalette pal

    
  
showBmp :: BmpBitmap -> IO ()
showBmp bmp = do 
    putStrLn $ render $ ppBmpHeader bmp
    putStrLn $ render $ ppBmpDibHeader bmp
    putStr "\n"
 
  


