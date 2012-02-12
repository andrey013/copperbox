{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Builtins.SignalGenerators
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Signal generating opcodes.
-- 
--------------------------------------------------------------------------------

module Orchsyn.Builtins.SignalGenerators
  (

  -- * Linear and exponential generators
    line


  , expon 
  , linseg
  , linsegr
  , expseg
  , expsegr
  , expsega

  , adsr
  , adsr_
  , madsr
  , madsr_
  , xadsr
  , xadsr_
  , mxadsr
  , mxadsr_

  -- * Table access
  , table
  , table_
  , tablei
  , tablei_
  , table3
  , table3_

  , oscil1
  , oscil1i
  , osciln

  -- * Phasors
  , phasor
  , phasor_
  , phasorbnk
  , phasorbnk_

  -- * Basic oscillators
  , oscil
  , oscil_
  , oscili
  , oscili_
  , oscil3
  , oscil3_
  , poscil
  , poscil_
  , poscil3
  , poscil3_
  , lfo
  , lfo_

  -- * Dynamic spectrum oscillators
  , buzz
  , buzz_
  , gbuzz
  , gbuzz_
  , vco

  -- * Additive synthesis / resynthesis
  , adsyn
  , adsynt
  , adsynt_
  , hsboscil
  , hsboscil_

  -- * FM synthesis
  , foscil
  , foscil_
  , foscili
  , foscili_
  , fmvoice
  , fmbell
  , fmrhode
  , fmwurlie
  , fmmetal
  , fmb3
  , fmpercfl

  -- * Sample playback - full spec versions todo
  , loscil
  , biloscil
  , loscil3
  , biloscil3
  , lposcil
  , lposcil3

  -- * Models and emulations
  , moog
  , shaker
  , marimba
  , vibes
  , mandol
  , gogobel
  , voice
  , lorenz
  , planet

  -- * Random noise generators
  , rand
  , randh
  , randi
  , linrand
  , trirand
  , exprand
  , bexprand
  , cauchy
  , pcauchy
  , poisson
  , gauss
  , weibull
  , betarand
  , unirand


  ) where


import Orchsyn.Language.Expr
import Orchsyn.OrchMonad


--------------------------------------------------------------------------------
-- Linear and Exponential generators





line ::(KA_Rate rate, MakeVar rate, TypeRate rate)
     => Expr IInit -> Expr IInit -> Expr IInit -> Instr (Expr rate)
line ia idur ib = 
    opcodeStmt1 "line" (rateOf undefined) args
  where
   args = [ uniRate ia, uniRate idur, uniRate ib ] 
    



expon ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Instr (Expr rate)
expon ia idur ib = 
    opcodeStmt1 "expon" (rateOf undefined) args
  where
   args = [uniRate ia, uniRate idur, uniRate ib ]



linseg ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> [(Expr IInit, Expr IInit)]  
       -> Instr (Expr rate)
linseg ia idur ib xs = 
    opcodeStmt1 "linseg" (rateOf undefined) args 
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
     
    



linsegr ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
        => Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Expr IInit ->  Expr IInit 
        -> Instr (Expr rate)
linsegr ia idur ib xs irel iz =
    opcodeStmt1 "linsegr" (rateOf undefined) args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest ++ end
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
    end  = [uniRate irel, uniRate iz]
     


expseg ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> [(Expr IInit, Expr IInit)]
       -> Instr (Expr rate)
expseg ia idur ib xs =
    opcodeStmt1 "expseg" (rateOf undefined) args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
     



expsegr ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
        => Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Expr IInit -> Expr IInit 
        -> Instr (Expr rate)
expsegr ia idur ib xs irel iz = 
    opcodeStmt1 "linsegr" (rateOf undefined) args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest ++ end
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
    end  = [uniRate irel, uniRate iz]
     




expsega :: Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Instr (Expr ARate)
expsega ia idur ib xs =
    opcodeStmt1 "expsega" (rateOf undefined) args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
     


adsr ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
     => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
     -> Instr (Expr rate)
adsr ia idec isl ir = 
    opcodeStmt1 "adsr" (rateOf undefined) args
  where
    args = [ uniRate ia,   uniRate idec, uniRate isl,  uniRate ir ]


adsr_ ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit
      -> Instr (Expr rate)
adsr_ ia idec isl ir idel =
    opcodeStmt1 "adsr" (rateOf undefined) args
  where
    args = [ uniRate ia,   uniRate idec
           , uniRate isl,  uniRate ir
           , uniRate idel ]



madsr ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Instr (Expr rate)
madsr ia idec isl ir =
    opcodeStmt1 "madsr" (rateOf undefined) args
  where
    args = [ uniRate ia,    uniRate idec
           , uniRate isl,   uniRate ir ]


madsr_ ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Instr (Expr rate)
madsr_ ia idec isl ir idel =
    opcodeStmt1 "madsr" (rateOf undefined) args
  where
    args = [ uniRate ia,    uniRate idec
           , uniRate isl,   uniRate ir
           , uniRate idel ]

xadsr ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Instr (Expr rate)
xadsr ia idec isl ir = 
    opcodeStmt1 "xadsr" (rateOf undefined) args
  where
    args = [ uniRate ia, uniRate idec, uniRate isl, uniRate ir ]


xadsr_ ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Instr (Expr rate)
xadsr_ ia idec isl ir idel =
    opcodeStmt1 "xadsr" (rateOf undefined) args 
  where
    args = [ uniRate ia,    uniRate idec
           , uniRate isl,   uniRate ir
           , uniRate idel ]


mxadsr ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Instr (Expr rate)
mxadsr ia idec isl ir =
    opcodeStmt1 "mxadsr" (rateOf undefined) args 
  where 
    args = [ uniRate ia,   uniRate idec
           , uniRate isl,  uniRate ir ]

mxadsr_ ::  (KA_Rate rate, MakeVar rate, TypeRate rate)
        => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Instr (Expr rate)
mxadsr_ ia idec isl ir idel = 
    opcodeStmt1 "mxadsr" (rateOf undefined) args 
  where
    args = [ uniRate ia,   uniRate idec
           , uniRate isl,  uniRate ir
           , uniRate idel ]


--------------------------------------------------------------------------------
-- Table access

table :: (Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
      => Expr rate1 -> Int -> Instr (Expr rate)
table ndx ifn =
    opcodeStmt1 "table" (rateOf undefined) args 
  where
    args = [ uniRate ndx, fromIntegral ifn ]


table_ :: (Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
       => Expr rate1 -> Int
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Instr (Expr rate)
table_ ndx ifn ixmode ixoff ixwrap =
    opcodeStmt1 "table" (rateOf undefined) args 
  where
    args = [ uniRate ndx,       fromIntegral ifn
           , uniRate ixmode,    uniRate ixoff
           , uniRate ixwrap ]


tablei :: (Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
       => Expr rate1 -> Int -> Instr (Expr rate)
tablei ndx ifn =
    opcodeStmt1 "tablei" (rateOf undefined) args 
  where
    args = [ uniRate ndx, fromIntegral ifn ]


tablei_ :: (Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
        => Expr rate1 -> Int
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Instr (Expr rate)
tablei_ ndx ifn ixmode ixoff ixwrap =
    opcodeStmt1 "tablei" (rateOf undefined) args 
  where
    args = [ uniRate ndx,       fromIntegral ifn
           , uniRate ixmode,    uniRate ixoff
           , uniRate ixwrap ]



table3 :: (Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
       => Expr rate1 -> Int -> Instr (Expr rate)
table3 ndx ifn =
    opcodeStmt1 "table" (rateOf undefined) args 
  where
    args = [ uniRate ndx, fromIntegral ifn ]


table3_ :: (Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
        => Expr rate1 -> Int 
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Instr (Expr rate)
table3_ ndx ifn ixmode ixoff ixwrap =
    opcodeStmt1 "table3" (rateOf undefined) args 
  where
    args = [ uniRate ndx,       fromIntegral ifn
           , uniRate ixmode,    uniRate ixoff
           , uniRate ixwrap ]


oscil1 :: Expr IInit -> Expr KRate -> Expr IInit -> Int
       -> Instr (Expr KRate)
oscil1 idel kamp idur ifn =
    opcodeStmt1 "oscil1" (rateOf undefined) args 
  where
    args = [ uniRate idel,  uniRate kamp
           , uniRate idur,  fromIntegral ifn ]


oscil1i :: Expr IInit -> Expr KRate -> Expr IInit -> Int
        -> Instr (Expr KRate)
oscil1i idel kamp idur ifn =
    opcodeStmt1 "oscil1i" (rateOf undefined) args 
  where
    args = [ uniRate idel,  uniRate kamp
           , uniRate idur,  fromIntegral ifn ]


osciln :: Expr KRate -> Expr IInit -> Int -> Expr IInit 
       -> Instr (Expr KRate)
osciln kamp ifrq ifn itimes =
    opcodeStmt1 "osciln" (rateOf undefined) args 
  where 
    args = [ uniRate kamp,      uniRate ifrq
           , fromIntegral ifn,  uniRate itimes ]


--------------------------------------------------------------------------------
-- Phasors

-- class CPhasor rate where


phasor :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
       => Expr rate1 -> Instr (Expr rate)
phasor cps =
    opcodeStmt1 "phasor" (rateOf undefined) args 
  where
    args = [ uniRate cps ]


phasor_ :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
        => Expr rate1 -> Expr IInit -> Instr (Expr rate)
phasor_ cps iphs =
    opcodeStmt1 "phasor" (rateOf undefined) args 
  where
    args = [ uniRate cps, uniRate iphs ]



phasorbnk :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
          => Expr rate1 -> Expr KRate -> Expr IInit 
          -> Instr (Expr rate)
phasorbnk cps kindx icnt =
    opcodeStmt1 "phasorbnk" (rateOf undefined) args 
  where
    args = [uniRate cps, uniRate kindx, uniRate icnt ]


phasorbnk_  :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate rate1)
            => Expr rate1 -> Expr KRate -> Expr IInit -> Expr IInit 
            -> Instr (Expr rate)
phasorbnk_ cps kindx icnt iphs =
    opcodeStmt1 "phasorbnk" (rateOf undefined) args 
  where
    args = [ uniRate cps,   uniRate kindx
           , uniRate icnt,  uniRate iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

-- | Note for A rate, cps can seemingly be any type.
-- 
oscil :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1, Rate r2)
      => Expr r1 -> Expr r2 -> Int -> Instr (Expr rate)
oscil amp cps ifn = 
    opcodeStmt1 "oscil" (rateOf undefined) args 
  where
    args = [ uniRate amp, uniRate cps, fromIntegral ifn ]


oscil_ :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int -> Expr IInit  
       -> Instr (Expr rate)
oscil_ amp cps ifn iphs =
    opcodeStmt1 "oscil" (rateOf undefined) args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


-- | Prefer @oscil@ rather than this one...
--
oscili :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int
       -> Instr (Expr rate)
oscili amp cps ifn = 
    opcodeStmt1 "oscili" (rateOf undefined) args 
  where
    args = [ uniRate amp, uniRate cps, fromIntegral ifn ]


oscili_ :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1, Rate r2)
        => Expr r1 -> Expr r2 -> Int -> Expr IInit 
        -> Instr (Expr rate)
oscili_ amp cps ifn iphs =
    opcodeStmt1 "oscili" (rateOf undefined) args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


oscil3 :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int
       -> Instr (Expr rate)
oscil3 amp cps ifn =
    opcodeStmt1 "oscil3" (rateOf undefined) args
  where
    args = [ uniRate amp, uniRate cps, fromIntegral ifn ]


oscil3_ :: (KA_Rate rate, MakeVar rate, TypeRate rate)
        => Expr r1 -> Expr r2 -> Int -> Expr IInit 
        -> Instr (Expr rate)
oscil3_ amp cps ifn iphs = 
    opcodeStmt1 "oscil3" (rateOf undefined) args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


poscil :: (KA_Rate rate, MakeVar rate, TypeRate rate)
       => Expr r1 -> Expr r2 -> Int 
       -> Instr (Expr rate)
poscil amp cps ifn  = 
    opcodeStmt1 "poscil" (rateOf undefined) args
  where 
    args = [uniRate amp, uniRate cps, fromIntegral ifn ]


poscil_ :: (KA_Rate rate, MakeVar rate, TypeRate rate)
        => Expr r1 -> Expr r2 -> Int -> Expr IInit 
        -> Instr (Expr rate)
poscil_ amp cps ifn iphs = 
    opcodeStmt1 "poscil" (rateOf undefined) args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


poscil3 :: (KA_Rate rate, MakeVar rate, TypeRate rate)
        => Expr r1 -> Expr r2 -> Int
        -> Instr (Expr rate)
poscil3 amp cps ifn = 
    opcodeStmt1 "poscil3" (rateOf undefined) args 
  where
    args = [ uniRate amp,  uniRate cps, fromIntegral ifn ]


poscil3_  :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate r1, Rate r2)
          => Expr r1 -> Expr r2 -> Int -> Expr IInit 
          -> Instr (Expr rate)
poscil3_ amp cps ifn iphs = 
    opcodeStmt1 "poscil3" (rateOf undefined) args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


lfo :: (KA_Rate rate, MakeVar rate, TypeRate rate)
    => Expr r1 -> Expr r2 -> Instr (Expr rate)
lfo amp cps = 
    opcodeStmt1 "lfo" (rateOf undefined) args 
  where
    args = [ uniRate amp, uniRate cps ]


lfo_ :: (KA_Rate rate, MakeVar rate, TypeRate rate)
     => Expr r1 -> Expr r2 -> Expr IInit 
     -> Instr (Expr rate)
lfo_ amp cps itype =
    opcodeStmt1 "lfo" (rateOf undefined) args 
  where
    args = [ uniRate amp, uniRate cps, uniRate itype ]

--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this
-- correct?

buzz :: Expr r1 -> Expr r2 -> Expr KRate -> Int  
     -> Instr (Expr ARate)
buzz xamp xcps knh ifn =
    opcodeStmt1 "buzz" (rateOf undefined) args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   fromIntegral ifn ]


buzz_ :: Expr r1 -> Expr r2 -> Expr KRate -> Int -> Expr IInit 
      -> Instr (Expr ARate)
buzz_ xamp xcps knh ifn iphs =
    opcodeStmt1 "buzz" (rateOf undefined) args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   fromIntegral ifn
           , uniRate iphs ]


gbuzz :: Expr r1 -> Expr r2 
      -> Expr KRate -> Expr KRate -> Expr KRate -> Int
      -> Instr (Expr ARate)
gbuzz xamp xcps knh klh kr ifn =
    opcodeStmt1 "gbuzz" (rateOf undefined) args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   uniRate klh
           , uniRate kr,    fromIntegral ifn ]


gbuzz_ :: Expr r1 -> Expr r2 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Int -> Expr IInit
       -> Instr (Expr ARate)
gbuzz_ xamp xcps knh klh kr ifn iphs =
    opcodeStmt1 "gbuzz" (rateOf undefined) args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   uniRate klh
           , uniRate kr,    fromIntegral ifn
           , uniRate iphs ]

vco :: Expr KRate -> Expr KRate -> Expr IInit -> Expr KRate 
    -> Int -> Expr IInit 
    -> Instr (Expr ARate)
vco kamp kfqc iwave kpw ifn imaxd =
    opcodeStmt1 "gbuzz" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfqc
           , uniRate iwave,     uniRate kpw
           , fromIntegral ifn,  uniRate imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Expr KRate -> Expr KRate -> Expr KRate -> String
      -> Instr (Expr ARate)
adsyn kamod kfmod ksmod ss =
    opcodeStmt1 "adsyn" (rateOf undefined) args 
  where
    args = [ uniRate kamod, uniRate kfmod
           , uniRate ksmod, StringE ss ]


adsynt :: Expr KRate -> Expr KRate 
       -> Int -> Int -> Int 
       -> Expr IInit  
       -> Instr (Expr ARate)
adsynt kamp kcps iwfn ifreqfn iampfn icnt =
    opcodeStmt1 "adsynt" (rateOf undefined) args 
  where
    args = [ uniRate kamp,        uniRate kcps
           , fromIntegral iwfn,   fromIntegral ifreqfn
           , fromIntegral iampfn, uniRate icnt ]


adsynt_ :: Expr KRate -> Expr KRate 
       -> Int -> Int -> Int 
       -> Expr IInit -> Expr IInit
       -> Instr (Expr ARate)
adsynt_ kamp kcps iwfn ifreqfn iampfn icnt iphs =
    opcodeStmt1 "adsynt" (rateOf undefined) args 
  where
    args = [ uniRate kamp,        uniRate kcps
           , fromIntegral iwfn,   fromIntegral ifreqfn
           , fromIntegral iampfn, uniRate icnt
           , uniRate iphs ]


hsboscil :: Expr KRate -> Expr KRate -> Expr KRate -> Expr IInit
         -> Int -> Int 
         -> Instr (Expr ARate)
hsboscil kamp ktone kbrite ibasfreq iwfn ioctfn = 
    opcodeStmt1 "hsboscil" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate ktone
           , uniRate kbrite,    uniRate ibasfreq
           , fromIntegral iwfn, fromIntegral ioctfn ]


hsboscil_ :: Expr KRate -> Expr KRate -> Expr KRate -> Expr IInit
          -> Int -> Int -> Expr IInit
          -> Instr (Expr ARate)
hsboscil_ kamp ktone kbrite ibasfreq iwfn ioctfn iphs = 
    opcodeStmt1 "hsboscil" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate ktone
           , uniRate kbrite,    uniRate ibasfreq
           , fromIntegral iwfn, fromIntegral ioctfn
           , uniRate iphs ]


--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Int 
       -> Instr (Expr ARate)
foscil xamp kcps kcar kmod kndx ifn =
    opcodeStmt1 "foscil" (rateOf undefined) args 
  where
    args = [ uniRate xamp,    uniRate kcps
           , uniRate kcar,    uniRate kmod
           , uniRate kndx,    fromIntegral ifn ]


foscil_ :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Int -> Expr IInit 
        -> Instr (Expr ARate)
foscil_ xamp kcps kcar kmod kndx ifn iphs =
    opcodeStmt1 "foscil" (rateOf undefined) args 
  where
    args = [ uniRate xamp,    uniRate kcps
           , uniRate kcar,    uniRate kmod
           , uniRate kndx,    fromIntegral ifn
           , uniRate iphs ]

foscili :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Int
        -> Instr (Expr ARate)
foscili xamp kcps kcar kmod kndx ifn =
    opcodeStmt1 "foscili" (rateOf undefined) args 
  where
    args = [ uniRate xamp,    uniRate kcps
           , uniRate kcar,    uniRate kmod
           , uniRate kndx,    fromIntegral ifn ]


foscili_ :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
         -> Int -> Expr IInit
         -> Instr (Expr ARate)
foscili_ xamp kcps kcar kmod kndx ifn iphs =
    opcodeStmt1 "foscil" (rateOf undefined) args 
  where
    args = [ uniRate xamp,  uniRate kcps
           , uniRate kcar,  uniRate kmod
           , uniRate kndx,  fromIntegral ifn
           , uniRate iphs ]


fmvoice :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Int -> Int -> Int -> Int -> Int 
        -> Instr (Expr ARate)
fmvoice kamp kfreq kvowel ktilt kvibamt kvibrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcodeStmt1 "fmvoice" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kvowel,    uniRate ktilt
           , uniRate kvibamt,   uniRate kvibrate
           , fromIntegral ifn1, fromIntegral ifn2
           , fromIntegral ifn3, fromIntegral ifn4
           , fromIntegral ivibfn ]


fmbell :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate
       -> Int -> Int -> Int -> Int -> Int
       -> Instr (Expr ARate)
fmbell kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmbell" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kc1,       uniRate kc2
           , uniRate kvdepth,   uniRate kvrate
           , fromIntegral ifn1, fromIntegral ifn2
           , fromIntegral ifn3, fromIntegral ifn4
           , fromIntegral ivibfn ]


fmrhode :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Int -> Int -> Int -> Int -> Int
        -> Instr (Expr ARate)
fmrhode kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmrhode" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kc1,       uniRate kc2
           , uniRate kvdepth,   uniRate kvrate
           , fromIntegral ifn1, fromIntegral ifn2
           , fromIntegral ifn3, fromIntegral ifn4
           , fromIntegral ivibfn ]


fmwurlie :: Expr KRate -> Expr KRate -> Expr KRate 
         -> Expr KRate -> Expr KRate -> Expr KRate
         -> Int -> Int -> Int -> Int -> Int 
         -> Instr (Expr ARate)
fmwurlie kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmwurlie" (rateOf undefined) args 
  where
   args = [ uniRate kamp,       uniRate kfreq
          , uniRate kc1,        uniRate kc2
          , uniRate kvdepth,    uniRate kvrate
          , fromIntegral ifn1,  fromIntegral ifn2
          , fromIntegral ifn3,  fromIntegral ifn4
          , fromIntegral ivibfn ]


fmmetal :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Int -> Int -> Int -> Int -> Int
        -> Instr (Expr ARate)
fmmetal kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmmetal" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kc1,       uniRate kc2
           , uniRate kvdepth,   uniRate kvrate
           , fromIntegral ifn1, fromIntegral ifn2
           , fromIntegral ifn3, fromIntegral ifn4
           , fromIntegral ivibfn ]



fmb3 :: Expr KRate -> Expr KRate -> Expr KRate 
     -> Expr KRate -> Expr KRate -> Expr KRate
     -> Int -> Int -> Int -> Int -> Int 
     -> Instr (Expr ARate)
fmb3 kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmb3" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kc1,       uniRate kc2
           , uniRate kvdepth,   uniRate kvrate
           , fromIntegral ifn1, fromIntegral ifn2
           , fromIntegral ifn3, fromIntegral ifn4
           , fromIntegral ivibfn ]


fmpercfl :: Expr KRate -> Expr KRate -> Expr KRate 
         -> Expr KRate -> Expr KRate -> Expr KRate        
         -> Int -> Int -> Int -> Int -> Int
         -> Instr (Expr ARate)
fmpercfl kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcodeStmt1 "fmpercfl" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kc1,       uniRate kc2
           , uniRate kvdepth,   uniRate kvrate
           , fromIntegral ifn1, fromIntegral ifn2
           , fromIntegral ifn3, fromIntegral ifn4
           , fromIntegral ivibfn ]

--------------------------------------------------------------------------------
-- Sample playback

-- Note - it seems idiomatic to want a stereo version with only
-- the mandatory args...


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil :: Expr rate -> Expr KRate -> Int
       -> Expr IInit -> Expr IInit
       -> Instr (Expr ARate)
loscil xamp kcps ifn ibase imod1 =
    opcodeStmt1 "loscil" (rateOf undefined) args
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate imod1 ]


-- | Stereo version of 'loscil'.
--
-- Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil :: Expr rate -> Expr KRate -> Int
         -> Expr IInit -> Expr IInit
         -> Instr (Expr ARate, Expr ARate)
biloscil xamp kcps ifn ibase imod1 =
    opcodeStmt2 "loscil" (rateOf undefined) args 
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate imod1 ]


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil3 :: Expr rate -> Expr KRate -> Int
        -> Expr IInit -> Expr IInit 
        -> Instr (Expr ARate)
loscil3 xamp kcps ifn ibase imod1 =
    opcodeStmt1 "loscil3" (rateOf undefined) args 
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate imod1 ]


-- | Stereo version of 'loscil3'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil3 :: Expr rate -> Expr KRate -> Int
          -> Expr IInit -> Expr IInit 
          -> Instr (Expr ARate, Expr ARate)
biloscil3 xamp kcps ifn ibase imod1 =
    opcodeStmt2 "loscil3" (rateOf undefined) args 
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate  imod1 ]



lposcil :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Int
        -> Instr (Expr ARate)
lposcil kamp kfreqrat kloop kend ifn =
    opcodeStmt1 "lposcil" (rateOf undefined) args 
  where
    args = [ uniRate kamp,    uniRate kfreqrat
           , uniRate kloop,   uniRate kend
           , fromIntegral ifn ]


lposcil3 :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
         -> Int
         -> Instr (Expr ARate)
lposcil3 kamp kfreqrat kloop kend ifn =
    opcodeStmt1 "lposcil3" (rateOf undefined) args 
  where
    args = [ uniRate kamp,     uniRate kfreqrat
           , uniRate kloop,    uniRate kend
           , fromIntegral ifn ]



--------------------------------------------------------------------------------
-- Models and emulations

moog :: Expr KRate -> Expr KRate -> Expr KRate 
     -> Expr KRate -> Expr KRate -> Expr KRate
     -> Int -> Int -> Int
     -> Instr (Expr ARate)
moog kamp kfreq kfiltq kfiltrate kvibf kvamp iafn iwfn ivfn =
    opcodeStmt1 "moog" (rateOf undefined) args
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kfiltq,    uniRate kfiltrate
           , uniRate kvibf,     uniRate kvamp
           , fromIntegral iafn, fromIntegral iwfn
           , fromIntegral ivfn ]


-- | idecay - default is 0
--
shaker :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr IInit
       -> Instr (Expr ARate)
shaker kamp kfreq kbeans kdamp ktimes idecay =
    opcodeStmt1 "shaker" (rateOf undefined) args 
  where
    args = [ uniRate kamp,     uniRate kfreq
           , uniRate kbeans,   uniRate kdamp
           , uniRate ktimes,   uniRate idecay ]


marimba :: Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr IInit 
        -> Int -> Expr KRate 
        -> Expr KRate -> Int
        -> Expr IInit
        -> Instr (Expr ARate)
marimba kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec =
    opcodeStmt1 "marimba" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate ihrd,      uniRate ipos
           , fromIntegral imp,  uniRate kvibf
           , uniRate kvamp,     fromIntegral ivibfn
           , uniRate idec ]


vibes :: Expr KRate -> Expr KRate 
      -> Expr IInit -> Expr IInit 
      -> Int -> Expr KRate 
      -> Expr KRate -> Int
      -> Expr IInit
      -> Instr (Expr ARate)
vibes kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec =
    opcodeStmt1 "vibes" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate ihrd,      uniRate ipos
           , fromIntegral imp,  uniRate kvibf
           , uniRate kvamp,     fromIntegral ivibfn
           , uniRate idec ]


-- | iminfreq - default 0
--
mandol :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Int -> Expr IInit
       -> Instr (Expr ARate)
mandol kamp kfreq kpluck kdetune kgain ksize ifn iminfreq =
    opcodeStmt1 "marimba" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kpluck,    uniRate kdetune
           , uniRate kgain,     uniRate ksize
           , fromIntegral ifn,  uniRate iminfreq ]


gogobel :: Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Expr KRate -> Expr KRate -> Int
        -> Instr (Expr ARate)
gogobel kamp kfreq ihrd ipos imp kvibf kvamp ivibfn  =
    opcodeStmt1 "gogobel" (rateOf undefined) args 
  where
    args = [ uniRate kamp,     uniRate kfreq
           , uniRate ihrd,     uniRate ipos
           , uniRate imp,      uniRate kvibf
           , uniRate kvamp,    fromIntegral ivibfn ]


voice :: Expr KRate -> Expr KRate -> Expr KRate 
      -> Expr KRate -> Expr KRate -> Expr KRate
      -> Int -> Int 
      -> Instr (Expr ARate)
voice kamp kfreq kphoneme kform kvibf kvamp ifn ivfn =
    opcodeStmt1 "voice" (rateOf undefined) args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kphoneme,  uniRate kform
           , uniRate kvibf,     uniRate kvamp
           , fromIntegral ifn,  fromIntegral ivfn ]


lorenz :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit
       -> Instr (Expr ARate, Expr ARate, Expr ARate)
lorenz ks kr kb kh ix iy iz ivx =
    opcodeStmt3 "lorenz" (rateOf undefined) args
  where
    args = [ uniRate ks, uniRate kr
           , uniRate kb, uniRate kh
           , uniRate ix, uniRate iy
           , uniRate iz, uniRate ivx ]


planet :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr IInit -> Expr IInit
       -> Instr (Expr ARate, Expr ARate, Expr ARate)
planet kmass1 kmass2 ksep ix iy iz ivx ivy ivz idelta ifriction =
    opcodeStmt3 "planet" (rateOf undefined) args 
  where
    args = [uniRate kmass1,   uniRate kmass2
           , uniRate ksep,     uniRate ix
           , uniRate iy,       uniRate iz
           , uniRate ivx,      uniRate ivy
           , uniRate ivz,      uniRate idelta
           , uniRate ifriction ]

--------------------------------------------------------------------------------
-- Random noise generators



rand :: (KA_Rate rate, MakeVar rate, TypeRate rate)
     => Expr rate -> Instr (Expr rate)
rand amp =
    opcodeStmt1 "rand" (rateOf undefined) args 
  where
    args = [ uniRate amp ]


randh :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate rate1, Rate rate2)
      => Expr rate1 -> Expr rate2 -> Instr (Expr rate)
randh amp cps =
    opcodeStmt1 "rand" (rateOf undefined) args 
  where
    args = [uniRate amp, uniRate cps ]


randi :: (KA_Rate rate, MakeVar rate, TypeRate rate, Rate rate1, Rate rate2)
      => Expr rate1 -> Expr rate2 -> Instr (Expr rate)
randi amp cps =
    opcodeStmt1 "rand" (rateOf undefined) args 
  where
    args = [ uniRate amp, uniRate cps ]


linrand ::  (Rate rate, MakeVar rate, TypeRate rate)
        => Expr KRate -> Instr (Expr rate)
linrand krange =
    opcodeStmt1 "linrand" (rateOf undefined) args 
  where
    args = [uniRate krange ]


trirand ::  (Rate rate, MakeVar rate, TypeRate rate)
        => Expr KRate -> Instr (Expr rate)
trirand krange =
    opcodeStmt1 "trirand" (rateOf undefined) args 
  where
    args = [ uniRate krange ]


exprand ::  (Rate rate, MakeVar rate, TypeRate rate)
        => Expr KRate -> Instr (Expr rate)
exprand krange =
    opcodeStmt1 "exprand" (rateOf undefined) args 
  where
    args = [ uniRate krange ]


bexprand ::  (Rate rate, MakeVar rate, TypeRate rate)
         => Expr KRate -> Instr (Expr rate)
bexprand krange =
    opcodeStmt1 "bexprand" (rateOf undefined) args 
  where
    args = [ uniRate krange ]


cauchy :: (Rate rate, MakeVar rate, TypeRate rate)
       => Expr KRate -> Instr (Expr rate)
cauchy kalpha =
    opcodeStmt1 "cauchy" (rateOf undefined) args 
  where
    args = [ uniRate kalpha ]


pcauchy :: (Rate rate, MakeVar rate, TypeRate rate)
        => Expr KRate -> Instr (Expr rate)
pcauchy kalpha =
    opcodeStmt1 "pcauchy" (rateOf undefined) args 
  where
    args = [ uniRate kalpha ]

poisson :: (Rate rate, MakeVar rate, TypeRate rate)
       => Expr KRate -> Instr (Expr rate)
poisson klambda =
    opcodeStmt1 "poisson" (rateOf undefined) args 
  where
    args = [ uniRate klambda ]


gauss :: (Rate rate, MakeVar rate, TypeRate rate)
      => Expr KRate -> Instr (Expr rate)
gauss krange =
    opcodeStmt1 "gauss" (rateOf undefined) args 
  where
   args = [ uniRate krange ]


weibull ::  (Rate rate, MakeVar rate, TypeRate rate)
        => Expr KRate -> Expr KRate -> Instr (Expr rate)
weibull ksigma ktau =
    opcodeStmt1 "weibull" (rateOf undefined) args 
  where
    args = [ uniRate ksigma, uniRate ktau ]


betarand ::  (Rate rate, MakeVar rate, TypeRate rate)
         => Expr KRate -> Expr KRate -> Expr KRate 
         -> Instr (Expr rate)
betarand krange kalpha kbeta =
    opcodeStmt1 "betarand" (rateOf undefined) args
  where
    args = [uniRate krange,  uniRate kalpha, uniRate kbeta ]


unirand :: (Rate rate, MakeVar rate, TypeRate rate)
        => KR -> Instr (Expr rate)
unirand krange =
    opcodeStmt1 "unirand" (rateOf undefined) args 
  where
    args = [ uniRate krange ]


