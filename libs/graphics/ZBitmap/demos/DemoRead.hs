{-# OPTIONS -Wall #-}

module Main where


import ZBitmap
import Text.PrettyPrint.HughesPJ ( render )


    
    

main :: IO ()
main = run24bit

run24bit :: IO ()
run24bit = do
    a <- readBmp "../images/24bit/luke.bmp"
    showBmp a
    let b = convertBmp a
    putStrLn $ showAsciiPicture $ makeAsciiPicture b
    -- maybe fk (sk a) (optPaletteSpecBmp a)
    let a' = bitmapToBmp24 b
    writeBmp "./out/luke_2.bmp" a'
    



runMono :: IO ()
runMono = do
    a <- readBmp "../images/mono/letterA.bmp"
    showBmp a
    let b = convertBmp a
    putStrLn $ showAsciiPicture $ makeAsciiPicture b
    maybe fk (sk a) (optPaletteSpecBmp a)
    let a' = bitmapToBmp24 b
    writeBmp "./out/letterA_2.bmp" a'

run8bit :: IO ()    
run8bit = do
    a <- readBmp "../images/8bit/picture256.bmp"
    showBmp a
    -- let b = monoTo24bit a
    -- putStrLn $ showAsciiPicture $ makeAsciiPicture b
    maybe fk (sk a) (optPaletteSpecBmp a)
    -- print b
    

fk :: IO ()
fk = putStrLn $ "No palette spec"

sk :: BmpBitmap -> BmpPaletteSpec -> IO ()
sk a ps = let pal = palette (bitsPerPixelBmp a) ps 
          in putStrLn $ render $ ppPalette pal  
  
showBmp :: BmpBitmap -> IO ()
showBmp bmp = do 
    putStrLn $ render $ ppBmpHeader bmp
    putStrLn $ render $ ppBmpDibHeader bmp
    putStr "\n"
 
  


