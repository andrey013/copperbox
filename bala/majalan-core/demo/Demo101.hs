{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module Demo101 where


import Majalan.Core

import System.Directory
import System.Process


main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeUnifiedFile "out/cs101.csd" flags orch01 (runScore env1 my_sco)
    _ <- system "csound out/cs101.csd"
    return ()
  where
    flags = flags_wav_file_out "out/cs101.wav"

    my_sco = sco01 `scoOver` sco01

data MyEnv = MyEnv  { instr101_num :: Int }

env1 = MyEnv { instr101_num = 101 }

instance Instr101 MyEnv where
  asksInstr101 e = instr101_num e


-- Ideally table statements should be /outside/ the frame.
--
-- Possibly an alternative to @frame@ would be useful where
-- the first param is a list of table statements.
--
sco01 :: Instr101 env => Score env
sco01 = withGens [ gen10 1 4096 [1] ] $ 
          frame [ i101    0 0.75 
                , i101    1 0.50
                , i101    2 0.25
                ]

-- | st * dur
--
i101 :: Instr101 env => Double -> Double -> Note env
i101 start dur = absNote asksInstr101 start dur [] 

class Instr101 env where
  asksInstr101 :: env -> Int


-- Note - frame should just take instr events. Taking tables as 
-- well is a special case.

orch01 :: [String]
orch01 = [instr101]

instr101 :: String
instr101 = unlines $ 
    [ "instr 101"
    , "a1      oscil       10000, 440, 1"
    , "        out         a1"
    , "endin"
    ]


-- Can scores support /link time/ resolution of instrument 
-- numbers (and f-tables)?
--

-- data Config = Config { instr101 :: Double }

-- sco01 = runScore (Config { instr101 = 101 }) $ ...


-- type Event = InstrNum -> ScoreStmt

-- data Instr101 = Instr101 deriving (Eq,Show)
  
-- newtype NewEvent = NE { getNE :: Int -> String }

-- NewEvent is wrong - every event has to be manually seeded with the 
-- correct Instr num.

-- data NewEvent key = NE key String

-- This is closer, all events for an instrument share the same key.
-- The key is used to resolve instr num from some dictionary.
-- 
-- Can we have an open universe of keys though? 
-- (Are String keys too ugly to contemplate?)


-- Is defining a class for reolving keys and its instances part 
-- of the score DSL?

-- Library

{-

newtype NewEvent key = NE [Int]

data Instr101

-- Client

class LocalResolveKey key where
   resolveKey :: NewEvent key -> [Int]

instance LocalResolveKey Instr101 where
   resolveKey (NE body) = 101 : body


instr101E :: Int -> Int -> NewEvent Instr101
instr101E st d = NE [st,d]

-- Wrong - this makes scores typed by events...
--
sco02 :: [NewEvent Instr101]
sco02 = [instr101E 0 1, instr101E 2 1]


-- Could each event define a reader monad asks function to get 
-- instr num from an environment?

data Env = Env { env_instr101 :: Int, env_instr102 :: Int }

env1 = Env { env_instr101 = 101, env_instr102 = 102 }

data Instr102

class Asks e instr where
  asks :: e -> instr -> Int

instance Asks Env Instr101 where
  asks (Env { env_instr101 = i }) _ = i
 

instance Asks Env Instr102 where
  asks (Env { env_instr102 = i }) _ = i



-- This looks better - just a single param type class ...

class Instr101E env where
  asks_Instr101 :: env -> Int

instance Instr101E Env where
  asks_Instr101 (Env { env_instr101 = i}) = i

-- newtype NewEvent2 env = NE2 { getNE2 :: env -> [CsValue] }

-}