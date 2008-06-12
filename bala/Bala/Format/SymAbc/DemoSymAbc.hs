

module DemoSymAbc where

import Bala.Format.SymAbc.AbcFormat

import Data.Ratio

-- these infix operators should be in a common file

infixl 7 #
(#) :: a -> (a -> b) -> b
x # f = f x

infixr 6 << 
(<<) ::(a -> b) -> a ->  b
f << a = f a 



demo_001 () =  firstRepeat +++ beginSlur


-- double attr application is unfortunately allowed
demo_002 () = rest # dur 2 # dur 4 +++ firstRepeat

demo_002a () = rest +++ firstRepeat

demo_003 () = note C # sharp # octaveHigh 2 +++ firstRepeat 
demo_003a () = sharp (note C) 

-- these two should fail if uncommented
-- demo_003b () = rest # octaveHigh 2
-- demo_003c () = rest # flat

demo_004 () =     book_ "My song book" 
              +++ area_ "area" 
              +++ tempo_ << stempo (1%2) 2 
              +++ meter_ << meter  (2%3)
              +++ key_ << key << keySpec c_ # locrian # dorian
              +++ history_ ["All tunes", "written in", "the past"]
              +++ defaultLength_ (2%4)
              +++ words_ "la di da"
              
              +++ abcmusic << x1
  where 
    x1 = note C # sharp # octaveHigh 2 +++ firstRepeat 
              
dem'' () = keySpec (c_ # sharp) # locrian


 