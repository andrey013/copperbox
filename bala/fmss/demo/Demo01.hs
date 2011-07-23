{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.Datatypes
import Sound.FMSS.Envelopes
import Sound.FMSS.SpecMonad
import Sound.FMSS.Translate
import Sound.FMSS.Utils.FormatCombinators

import Data.String

-- | Overloaded strings permit a lexically bizarre but otherwise 
-- fairly simple approach to embedded expressions.

outS :: Stmt
outS = Out $ "iamp" * "kampenv" * mkCarrier 1

mkCarrier :: Int -> Expr
mkCarrier i = fromString $ "acar" ++ show i ++ "sig"

iamp :: Expr
iamp = "iamp"

demo011 :: IO ()
demo011 = print $ expsegEnvelope [ (0,0), (0.1,1), (10,0.6), (25,0.3)
                                 , (50,0.15), (90,0.1), (100,0)
                                 ]

env1 :: (String,[SymDouble])
env1 = linsegEnvelope [ (0,0), (0.1,1), (10,0.6), (25,0.3)
                      , (50,0.15), (90,0.1), (100,0)
                      ]

osc1 :: Carrier
osc1 = Carrier 6 $ Oscil (BaseScaler 2.4) Nothing


demo01 = either print print $ (fmap (format) $ translate fm1)

-- Try to make a synth a function consuming oscillators...


-- | Configuration of 6 oscillators like the DX7.


data Config6 = Config6
      { oscil6_1 :: Oscil
      , oscil6_2 :: Oscil
      , oscil6_3 :: Oscil
      , oscil6_4 :: Oscil
      , oscil6_5 :: Oscil
      , oscil6_6 :: Oscil
      }



algo28 :: Config6 -> Spec ()
algo28 (Config6 o1 o2 o3 o4 o5 o6) = do
    m2 <- modulator o2
    m5 <- modulator o5
    m4 <- modulator o4
    c1 <- carrier   o1
    c3 <- carrier   o3
    c6 <- carrier   o6
    m2 =>- c1
    m5 =>= m4 
    m4 =>- c3
    -- m5 should be cycled
    return ()





fm1 :: FMSynth
fm1 = FMSynth { fm_instr_num  = 1
              , fm_sinetbl    = 1
              , fm_envelopes  = [ampenv]
              , fm_synth_body = SynthBody { synth_mods       = [om]
                                          , synth_cars       = [oc]
                                          , synth_links      = [ModCar 1 1]
                                          }
              , fm_out        = outS
              }

  where
    om = Modulator 1 $ Oscil (BaseScaler 1.4) Nothing -- (Just $ \a -> iamp * a)
    oc = Carrier 1 $ Oscil  (BaseScaler 1.0) Nothing
    
    ampenv  = EnvelopeSpec "kampenv" Nothing (fst env1) (snd env1)


