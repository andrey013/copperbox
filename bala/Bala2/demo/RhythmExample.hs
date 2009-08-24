
-- ghci ...
-- :set -i../src:../../Mullein/src


module RhythmExample where

import Bala.RhythmPattern

import Mullein.LilyPond hiding ( Pulse )

import qualified Data.Stream              as WS
import qualified Data.Stream.Hinze.Stream as HS
import Text.PrettyPrint.Leijen


import qualified Data.Set                 as Set


demo0 = showBox dus

dus :: SubsetPattern
dus = makeSubsetPattern s 16 where
  s = [0,2,5,7,10,13]

afoxePat :: SubsetPattern
afoxePat = makeSubsetPattern s 16 where
  s = [1,3,5,7, 9,11,13,15]



afoxe :: [PDGlyph]
afoxe = interpret rest (take 20 $ cycle [f8,f16]) afoxePat where
  f8               = adapt 2 (\_ -> c 5 en)  
  f16              = adapt 1 (\_ -> c 5 sn)

  rest 1           = snr
  rest 2           = enr
  rest n           = error $ "unrecognized rest duration " ++ show n


  adapt n fn i
      | n == i     = (Nothing, fn n) 
      | n <  i     = (Just $ i-n, fn n)
      | otherwise  = error $ "Too small - saw " ++ show i ++ " expecting " ++ show n
                                      

twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4


demo1 :: Doc
demo1 =  version "2.12.2" 
     <$> score (relative middle_c $ key c_nat "major" <$> time 2 4 <$> tune)
  where
    tune = simpleOutput $ renderPhrase $ rewritePitch middle_c $ rewriteDuration xs
    xs   = phrase twoFourTime afoxe


output1 :: IO ()
output1 = do 
  writeDoc "afoxe.ly"  demo1


dummy = quickRhythm dus
  

quickRhythm = fork (id,WS.take 24) . toStream' where 
  fork (f,g) (a,b) = (f a, g b)



--afoxe2 :: [PDGlyph]
afoxe2 = prefix ana $ cross (WS.cycle [2,1]) rs 
  where
    (ana,rs) = toStream' afoxePat
    prefix a | a > 0     = (Left a HS.<:) 
             | otherwise = id



afoxePat2 :: RhythmPattern
afoxePat2 = RhythmPattern s where
  s = [R 1, N 2,N 1]

dummy2 = showBox $ toSubsetPattern afoxePat2

quick2 = WS.take 24 

afoxe3 :: [PDGlyph]
afoxe3 = fmap fn $ takeNBars 4 8 $ toStream afoxePat2
  where
   fn (R 1) = snr
   fn (N 1) = c 5 sn
   fn (N 2) = c 5 en

demo2 :: Doc
demo2 =  version "2.12.2" 
     <$> score (relative middle_c $ key c_nat "major" <$> time 2 4 <$> tune)
  where
    tune = simpleOutput $ renderPhrase $ rewritePitch middle_c $ rewriteDuration xs
    xs   = phrase twoFourTime afoxe3
