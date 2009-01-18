

module Demo where


import ReadBmp
import Types 


main = do
  a <- readBmp "letterA.bmp"
  showBmp a
  
showBmp :: BMPfile -> IO ()
showBmp (BMPfile h dh body) = do 
    putStrLn $ show h
    putStrLn $ show dh
    mapM_ (putStrLn . show) body
    
    


  
