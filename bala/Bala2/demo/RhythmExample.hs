
-- ghci ...
-- :set -i../src:../../Mullein/src


module RhythmExample where

import Bala.RhythmPattern

import Mullein.LilyPond

import Text.PrettyPrint.Leijen


import qualified Data.Set as Set


demo0 = showBox dus

dus :: RhythmPattern
dus = makeRhythmPattern s 16 where
  s = [0,2,5,7,10,13]

afoxePat :: RhythmPattern
afoxePat = makeRhythmPattern s 16 where
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
