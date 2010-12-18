{-# OPTIONS -Wall #-}


module Demo01 where

import ZMidi.Construction

import qualified ZMidi.Construction.Utils.JoinList as JL

import ZMidi.Core


main :: IO ()
main = demo01 >> demo02

-- default tempo is 120 beats per minute

demo01 :: IO ()
demo01 = do
    putStrLn "Writing demo01.mid..."
    writeMidiMCT "demo01.mid" $ (JL.one (JL.one section1))
  where
    section1 = section 120 [voice1, voice2]
    instr    = instrumentNumber Honky_tonk
    voice1   = instrument instr >> note dquarter (c_nat 4) 
                                >> note dquarter (c_nat 4)
    voice2   = instrument instr >> note deighth  (e_nat 4) 
                                >> note deighth  (g_nat 4)


-- how to append sections? - can now use mappend...

section :: Double -> [Build a] -> Section
section bpm voices = Section bpm $ JL.fromListF fn voices
  where
    fn = SectionVoice . execBuild build_env_zero
   


temp01 = runBuild build_env_zero $ 
    note dquarter (c_nat 4) >> note dquarter (e_nat 4) >> rest dquarter
    

demo02 :: IO ()
demo02 = do
    ans <- readMidi "demo01.mid"
    case ans of
      Left (n,msg) -> putStrLn $ "Parse failure at " ++ show n ++ ": " ++ msg
      Right m      -> printMidi m

