{-# OPTIONS -Wall #-}

module FmBell where


import Sound.FMSS
import Sound.FMSS.Datatypes

import System.Directory


-- | Configuration of 7 oscillators.


data Config7 = Config7
      { oscil7_1 :: Oscil
      , oscil7_2 :: Oscil
      , oscil7_3 :: Oscil
      , oscil7_4 :: Oscil
      , oscil7_5 :: Oscil
      , oscil7_6 :: Oscil
      , oscil7_7 :: Oscil
      }


-- Probably algorithms should return carriers so they 
-- can be used in Out specs

algorithm :: Config7 -> Config Out3
algorithm (Config7 o1m o2m o3m o4m o5c o6c o7c) = do
    m1 <- modulator o1m
    m2 <- modulator o2m
    m3 <- modulator o3m
    m4 <- modulator o4m
    c1 <- carrier   o5c
    c2 <- carrier   o6c
    c3 <- carrier   o7c
    m1 =>- c1
    m2 =>- c2
    m3 =>- c2
    m4 =>- c3
    return $ out3 (c1,c2,c3)

    -- m2 and m3 are summed to make the c2 carrier.


indxenv :: Expseg
indxenv = expsegEnvelope [ (0.0, 1.0), (100,0.001) ]

ampenv :: Expseg 
ampenv = expsegEnvelope [ (0.0,0.01), (0.02,1.0), (99.98,0.01) ]


fmBell :: Spec SpecAns
fmBell = do
    (_,iamp,ifreq)      <- globals
    indx                <- variable "indx"      (pfield 6)
    ifm1index           <- variable "ifm1index" (32.0 * ifreq)
    ifm2index           <- variable "ifm2index" (4.0 * (8.0 - ifreq / 50.0))
    ifm3index           <- variable "ifm3index" (ifm2index * 0.705 * 
                                                   (1.4 - ifreq / 250.0))

    ifm4index           <- variable "ifm4index" (32.0 * (20.0 - ifreq / 20.0))

    kindxenv            <- envelope "kindexenv" indxenv
    kampenv             <- envelope "kampenv"   ampenv
    
    let mod1pp = mult (ifm1index * indx * kindxenv)

    config (algorithm $ Config7 mod1 mod2 mod3 mod4 car1 car2 car3)
           (\(c1,c2,c3) -> iamp * kampenv * (c1 + (0.15 * c2) + (0.15 * c3)))
  where
    mod1 = Oscil (BaseFreqScaler (* 2.0))   Nothing
    mod2 = Oscil (BaseFreqScaler (* 1.41))  Nothing
    mod3 = Oscil (BaseFreqScaler (* 2.82))  Nothing
    mod4 = Oscil (BaseFreqScaler (* 2.4))   Nothing
    car1 = Oscil (BaseFreqScaler id)        Nothing
    car2 = Oscil (BaseFreqScaler id)        Nothing
    car3 = Oscil (BaseFreqScaler (* 2.4))   Nothing

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeSynth "out/fmbell.orc" 
               (Params { instr_num = 1, sine_table = 1}) 
               fmBell


{-

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


-- Note - the cycle shouldn't use a VarE directly, some more 
-- thought is needed here...
--
config2_no1 :: Config2 -> Config Out1
config2_no1 (Config2 o1 o2) = do
   m1 <- modulator o1
   c1 <- carrier   o2
   m1 =>- c1
   cycleMM m1 m1 (\e -> e * VarE "indx")
   return $ out1 c1


fm1 :: FMSynth
fm1 = FMSynth { fm_instr_num  = 1
              , fm_sinetbl    = 1
              , fm_decls      = [ampenv]
              , fm_synth_body = SynthBody { synth_mods       = [om]
                                          , synth_cars       = [oc]
                                          , synth_links      = [ModCar 1 1]
                                          , synth_cycles     = []
                                          }
              , fm_out        = outS
              }

  where
    om = Modulator 1 $ Oscil (BaseScaler 1.4) Nothing -- (Just $ \a -> iamp * a)
    oc = Carrier 1 $ Oscil  (BaseScaler 1.0) Nothing
    
    ampenv  = let (name,args) = deconsEnvelope env1
              in CommentD "amplitude" $ Envelope "kampenv" name args



fm2 :: Spec SpecAns
fm2 = do
    ampenv <- envelope "kampenv" env1
    config (config2_no1 $ Config2 oscil1 oscil2)
           (\c1 -> iamp * ampenv * c1)
  where
    oscil1 = Oscil (BaseScaler 1.4) Nothing
    oscil2 = Oscil (BaseScaler 1.0) Nothing

demo02 = writeSynth "fm-one.orc" (Params { instr_num = 1, sine_table = 1}) fm2


-}