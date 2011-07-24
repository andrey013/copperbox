{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS -Wall #-}

module Demo01 where

import Sound.FMSS.AbstractSyntax
import Sound.FMSS.ConfigMonad
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

env1 :: Linseg
env1 = linsegEnvelope [ (0,0), (0.1,1), (10,0.6), (25,0.3)
                      , (50,0.15), (90,0.1), (100,0)
                      ]

osc1 :: Carrier
osc1 = Carrier 6 $ Oscil (BaseScaler 2.4) Nothing


demo01 = either print print $ (fmap (format) $ translate fm1)



-- | Configuration of 6 oscillators like the DX7.


data Config6 = Config6
      { oscil6_1 :: Oscil
      , oscil6_2 :: Oscil
      , oscil6_3 :: Oscil
      , oscil6_4 :: Oscil
      , oscil6_5 :: Oscil
      , oscil6_6 :: Oscil
      }


-- Probably algorithms should return carriers so they 
-- can be used in Out specs

algo28 :: Config6 -> Config Out3
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
    return $ out3 (c1,c3,c6)


data Config2 = Config2
      { oscil2_1 :: Oscil
      , oscil2_2 :: Oscil
      }



config2_no1 :: Config2 -> Config Out1
config2_no1 (Config2 o1 o2) = do
   m1 <- modulator o1
   c1 <- carrier   o2
   m1 =>- c1
   return $ out1 c1


fm1 :: FMSynth
fm1 = FMSynth { fm_instr_num  = 1
              , fm_sinetbl    = 1
              , fm_decls      = [ampenv]
              , fm_synth_body = SynthBody { synth_mods       = [om]
                                          , synth_cars       = [oc]
                                          , synth_links      = [ModCar 1 1]
                                          }
              , fm_out        = outS
              }

  where
    om = Modulator 1 $ Oscil (BaseScaler 1.4) Nothing -- (Just $ \a -> iamp * a)
    oc = Carrier 1 $ Oscil  (BaseScaler 1.0) Nothing
    
    ampenv  = let (name,args) = deconsEnvelope env1
              in CommentD "amplitude" $ Envelope "kampenv" name args



fm2 :: FMSynth
fm2 = execSpec (Params { instr_num = 1, sine_table = 1}) $ do
    ampenv <- envelope "kampenv" env1
    config (config2_no1 $ Config2 oscil1 oscil2)
           (\c1 -> iamp * ampenv * c1)
  where
    oscil1 = Oscil (BaseScaler 1.4) Nothing
    oscil2 = Oscil (BaseScaler 1.0) Nothing

demo02 = either print print $ (fmap (format) $ translate fm1)