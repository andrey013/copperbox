{-# OPTIONS -Wall #-}

module Main where


import ZBmp1

import Text.PrettyPrint.HughesPJ ( render )

main :: IO ()
main = do
  a <- readBmp "letterA.bmp"
  -- a <- readBmp "picture8x8.bmp"
  showBmp a
  
showBmp :: BMPfile -> IO ()
showBmp bmp@(BMPfile h dh pals body) = do 
    putStrLn $ render $ ppBMPheader h
    putStrLn $ render $ ppV3DibHeader dh
    putStr "\n"
    putStrLn $ render $ ppBMPbody (_dib_width dh) (_dib_height dh) body
    putStr "\n"
 --   putStrLn $ show $ body
    -- writeBmp "luke_2.bmp" bmp

  
