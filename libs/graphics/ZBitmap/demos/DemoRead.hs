{-# OPTIONS -Wall #-}

module Main where


import ZBitmap
import Text.PrettyPrint.HughesPJ ( render )

main :: IO ()
main = do
    -- a <- readBmp "../images/24bit/luke.bmp"
    -- a <- readBmp "../images/24bit/letterA.bmp"
    a <- readBmp "../images/8bit/picture256.bmp"
    -- a <- readBmp "../images/mono/letterA.bmp"
    showBmp a
  --  let b = dibToBitmap a
  --  putStrLn $ showAsciiPicture $ makeAsciiPicture b
    maybe fk (sk a) (_opt_palette a)
  where
    fk = putStrLn $ "No palette spec"
    sk a ps = let pal = palette (getBPP a) ps 
              in putStrLn $ render $ ppPalette pal

getBPP (BMPfile _ dib _ _) = _bits_per_pixel dib      
  
showBmp :: BMPfile -> IO ()
showBmp bmp@(BMPfile h dh _ body) = do 
    putStrLn $ render $ ppBMPheader h
    putStrLn $ render $ ppV3DibHeader dh
    putStr "\n"
 
  


