{-# OPTIONS -Wall #-}

module FmBell where


import Sound.FMSS

import System.Directory



data ConfigBell = ConfigBell
      { mod1_input      :: Expr
      , mod1_postpro    :: Expr -> Expr
      , mod2_input      :: Expr
      , mod3_input      :: Expr
      , mod2_3_combine  :: Expr -> Expr -> Expr
      , mod4_input      :: Expr
      , mod4_postpro    :: Expr -> Expr
      , car1_input      :: Expr -> Expr
      , car2_input      :: Expr -> Expr
      , car3_input      :: Expr -> Expr
      , output_spec     :: Expr -> Expr -> Expr -> Expr 
      }

algorithm :: ConfigBell -> Config ()
algorithm (ConfigBell { mod1_input = min1E, mod2_input = min2E 
                      , mod3_input = min3E, mod4_input = min4E
                      , car1_input = cin1E, car2_input = cin2E
                      , car3_input = cin3E
                      , mod1_postpro = m1ppE
                      , mod4_postpro = m4ppE
                      , mod2_3_combine = m23combE
                      , output_spec = outE }) = do

    m1 <- postpro m1ppE $ modulator min1E
    c1 <- carrier $ expr1 m1 cin1E

    m2 <- modulator min2E
    m3 <- postpro (m23combE (vexpr m2)) $ modulator min3E
    c2 <- carrier $ expr1 m3 cin2E


    m4 <- postpro m4ppE $ modulator min4E
    c3 <- carrier $ expr1 m4 cin3E
  
    out $ expr3 c1 c2 c3 outE
    return ()



indxenv :: Expseg
indxenv = expsegEnvelope [ (0.0, 1.0), (100,0.001) ]

ampenv :: Expseg 
ampenv = expsegEnvelope [ (0.0,0.01), (0.02,1.0), (99.98,0.01) ]



-- Currently Spec and COnfig treat variables differently...

fmBell :: Spec ()
fmBell = do
    (_,iamp,ifreq)      <- globals
    indx                <- variable "indx"      (pfield 6)
    ifm1index           <- variable "ifm1index" (32.0 * ifreq)
    ifm2index           <- variable "ifm2index" (4.0 * (8.0 - ifreq / 50.0))
    ifm3index           <- variable "ifm3index" (ifm2index * 0.705 * 
                                                   (1.4 - ifreq / 250.0))

    ifm4index           <- variable "ifm4index" (32.0 * (20.0 - ifreq / 20.0))
    
    imod1freq           <- variable "imod1freq" (ifreq * 2.0)
    imod2freq           <- variable "imod2freq" (ifreq * 1.41)
    imod3freq           <- variable "imod3freq" (ifreq * 2.82)
    imod4freq           <- variable "imod4freq" (ifreq * 2.4)
    icar1freq           <- variable "icar1freq" ifreq
    icar2freq           <- variable "icar2freq" ifreq
    icar3freq           <- variable "icar3freq" (ifreq * 2.4)


    kindxenv            <- envelope "kindexenv" indxenv
    kampenv             <- envelope "kampenv"   ampenv
    
    let mod1pp   = ((ifm1index * indx * kindxenv) *)
    let m23comb  = \m2 m3 -> kindxenv * ((ifm2index * m2) + (ifm3index * m3))
    let mod4pp   = ((ifm4index * indx * kindxenv) *)
    let out_spec = \c1 c2 c3 -> iamp * kampenv * (c1 + (0.15 * c2) + (0.15 * c3))

    config (algorithm $ ConfigBell
                          { mod1_input      = imod1freq
                          , mod1_postpro    = mod1pp
                          , mod2_input      = imod2freq
                          , mod3_input      = imod3freq
                          , mod2_3_combine  = m23comb
                          , mod4_input      = imod4freq
                          , mod4_postpro    = mod4pp
                          , car1_input      = (icar1freq +)
                          , car2_input      = (icar2freq +)
                          , car3_input      = (icar3freq +)
                          , output_spec     = out_spec
                          })
    return ()

main :: IO ()
main = do
    createDirectoryIfMissing True "./out/" 
    writeSynth "out/fmbell.orc" 
               (Params { instr_num = 1, sine_table = 1}) 
               fmBell


