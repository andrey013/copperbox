{-# OPTIONS -Wall #-}

module Main where


import ZBitmap
import Text.PrettyPrint.HughesPJ ( render )


main :: IO ()
main = run4bit
    
runMono :: IO ()
runMono = 
    runAction ("../images/mono/letterA.bmp", "./out/letterA_2.bmp", True)

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
    let b = convertBmp a
    putStrLn $ showAsciiPicture $ makeAsciiPicture b
    if show_palette 
      then maybe fk (sk a) (optPaletteSpecBmp a)
      else putStrLn "No palette"
    let a' = bitmapToBmp24 b
    writeBmp outfile a'
  where
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
    



