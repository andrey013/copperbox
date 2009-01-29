{-# OPTIONS -Wall #-}

module Main where


import ZBitmap
import Text.PrettyPrint.HughesPJ ( render )


dmono :: Int -> Int
dmono w = 8 * (((1 * w) + 7) `div` 8) 

d4bit :: Int -> Int
d4bit w = 2 * (((4 * w) + 7) `div` 8) 

d8bit :: Int -> Int
d8bit w = 1 * (((8 * w) + 7) `div` 8) 

d16bit :: Int -> Int
d16bit w = 1 * (((16 * w) + 7) `div` 8) 

d24bit :: Int -> Int
d24bit w = 1 * (((24 * w) + 7) `div` 8)

d32bit :: Int -> Int
d32bit w = 1 * (((32 * w) + 7) `div` 8)

dn :: Int ->Int -> Int
dn n w  = z * (((n * w) + 7) `div` 8) where
    z = if n == 1 then 8 else (if n == 4 then 2 else 1)
    
    

main :: IO ()
main = runMono

run24bit = do
    a <- readBmp "../images/24bit/luke.bmp"
    showBmp a
    let b = dibToBitmap a
    putStrLn $ showAsciiPicture $ makeAsciiPicture b
    maybe fk (sk a) (optPaletteSpecBmp a)




runMono = do
    a <- readBmp "../images/mono/letterA.bmp"
    showBmp a
    let b = monoTo24bit a
    putStrLn $ showAsciiPicture $ makeAsciiPicture b
    maybe fk (sk a) (optPaletteSpecBmp a)
    
fk = putStrLn $ "No palette spec"
sk a ps = let pal = palette (bitsPerPixelBmp a) ps 
          in putStrLn $ render $ ppPalette pal  
  
showBmp :: BmpBitmap -> IO ()
showBmp bmp = do 
    putStrLn $ render $ ppBmpHeader bmp
    putStrLn $ render $ ppBmpDibHeader bmp
    putStr "\n"
 
  


