

module ChordFingerings where

import Data.List
import Text.PrettyPrint.Leijen

x :: Int
x = (-1)

type Fingering = [Int]
type Fretting  = [Int]

data Barre = NoBarre | BarreStart | BarreEnd
  deriving (Eq,Show)
  
type Chord = [(Int,Maybe Int,Barre)]

finger :: Fretting -> Fingering -> Chord
finger xs ys = step xs (barre ys)
  where 
    step []     _             = []
    step (x:xs) []            = (x,Nothing, NoBarre) : step xs [] 
    step (x:xs) ((a,b):ys)  
            | x <= 0          = (x,Nothing, NoBarre) : step xs ((a,b):ys)
            | otherwise       = (x, Just a, b) : step xs ys  
    



terse :: Chord -> Doc
terse = cat . map interp
  where 
    interp (i,_      ,_)  | i <  0  = text "x;"
    interp (i,_      ,_)  | i == 0  = text "o;"
    interp (i,Just j ,b)            = (ppbarre b $ intdashint i j) <> semicolon
    interp (i,Nothing,b)            = intdashint i 0 <> semicolon
    
    intdashint i j        = int i <> dash <> int j
    
    ppbarre NoBarre    d  = d
    ppbarre BarreStart d  = d <> dash <> lparen
    ppbarre BarreEnd   d  = d <> dash <> rparen

dash, semicolon :: Doc
dash      = char '-'
semicolon = char ';'



barre :: Fingering -> [(Int,Barre)]
barre xs = step xs (repeated xs) []
  where
    step []     bs zs  = []
    step (x:xs) bs zs  
        | x `elem` bs && (x `nelem` zs) = (x,BarreStart) : step xs bs (x:zs)
        | x `elem` bs && (x `nelem` xs) = (x,BarreEnd)   : step xs bs zs
        | otherwise                     = (x,NoBarre)    : step xs bs zs        
  
    nelem x xs = not $ elem x xs 
  
repeated :: Eq a => [a] -> [a]
repeated = nub . step
  where
    step []                     = []
    step (x:xs) | x `elem` xs   = x : step xs
                | otherwise     = step xs     

d_fingering :: Fingering
d_fingering = [1,2,3]

d_fretting :: Fretting
d_fretting  = [x,x,0,2,3,2]

demo1 = terse $ d_fretting `finger` d_fingering
demo2 = terse $ [7,9,7,8,x,x] `finger` [1,3,1,2]
 