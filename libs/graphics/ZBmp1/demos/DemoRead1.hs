{-# OPTIONS -Wall #-}

module Main where


import ZBmp1

import Data.Array.IArray
import Text.PrettyPrint.HughesPJ ( render )

main :: IO ()
main = do
  -- a <- readBmp "letterA.bmp"
  -- a <- readBmp "picture8x8.bmp"
  -- a <- readBmp "mono8x8.bmp"
  a <- readBmp "lineW33H1.bmp" 
  
  showBmp a
  
showBmp :: BMPfile -> IO ()
showBmp bmp@(BMPfile h dh pals body) = do 
    putStrLn $ render $ ppBMPheader h
    putStrLn $ render $ ppV3DibHeader dh
    putStr "\n"
    putStrLn $ render $ ppBMPbody (_bmp_width dh) (_bmp_height dh) body
    putStr "\n"
    showBounds body
    let pic = makeAsciiPicture (_bmp_height dh) (_bmp_width dh) (getMono body)
    putStrLn $ showAsciiPicture pic
    putStrLn $ show $ bounds pic
    printCBBoxA pic
    
    
    
 --   putStrLn $ show $ body
    -- writeBmp "luke_2.bmp" bmp


getMono :: BMPbody -> ImageData 
getMono UnrecognizedFormat = error "Unrecognized Format"
getMono (Mono img)         = img
  
showBounds :: BMPbody -> IO ()
showBounds UnrecognizedFormat = putStrLn $ "__"
showBounds (Mono arr)         = putStrLn $ show $ bounds arr

dummy_ascii_picture :: AsciiPicture
dummy_ascii_picture = listArray ((0,0),(3,2)) $ "012" ++ "012" ++ "012" ++ "012"

d01 = putStrLn $ showAsciiPicture $ blankAsciiPicture 3 4

