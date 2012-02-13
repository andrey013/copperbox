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





line :: KA_Rate rate
     => Expr IInit -> Expr IInit -> Expr IInit -> Opcode1 rate
line ia idur ib = 
    opcodeStmt1 "line" args
  where
   args = [ uniRate ia, uniRate idur, uniRate ib ] 
    



expon :: KA_Rate rate
      => Expr IInit -> Expr IInit -> Expr IInit -> Opcode1 rate
expon ia idur ib = 
    opcodeStmt1 "expon" args
  where
   args = [uniRate ia, uniRate idur, uniRate ib ]



linseg :: KA_Rate rate
       => Expr IInit -> Expr IInit -> Expr IInit -> [(Expr IInit, Expr IInit)]  
       -> Opcode1 rate
linseg ia idur ib xs = 
    opcodeStmt1 "linseg" args 
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
     
    



linsegr :: KA_Rate rate
        => Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Expr IInit ->  Expr IInit 
        -> Opcode1 rate
linsegr ia idur ib xs irel iz =
    opcodeStmt1 "linsegr" args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest ++ end
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
    end  = [uniRate irel, uniRate iz]
     


expseg :: KA_Rate rate
       => Expr IInit -> Expr IInit -> Expr IInit -> [(Expr IInit, Expr IInit)]
       -> Opcode1 rate
expseg ia idur ib xs =
    opcodeStmt1 "expseg" args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
     



expsegr ::  KA_Rate rate
        => Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
expsegr ia idur ib xs irel iz = 
    opcodeStmt1 "linsegr" args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest ++ end
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
    end  = [uniRate irel, uniRate iz]
     




expsega :: Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Opcode1 ARate
expsega ia idur ib xs =
    opcodeStmt1 "expsega" args
  where
    args = uniRate ia : uniRate idur : uniRate ib : rest
    rest = concatMap (\(a,b) -> [uniRate a, uniRate b]) xs
     


adsr :: KA_Rate rate
     => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
     -> Opcode1 rate
adsr ia idec isl ir = 
    opcodeStmt1 "adsr" args
  where
    args = [ uniRate ia,   uniRate idec, uniRate isl,  uniRate ir ]


adsr_ :: KA_Rate rate
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit
      -> Opcode1 rate
adsr_ ia idec isl ir idel =
    opcodeStmt1 "adsr" args
  where
    args = [ uniRate ia,   uniRate idec
           , uniRate isl,  uniRate ir
           , uniRate idel ]



madsr ::  KA_Rate rate
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 rate
madsr ia idec isl ir =
    opcodeStmt1 "madsr" args
  where
    args = [ uniRate ia,    uniRate idec
           , uniRate isl,   uniRate ir ]


madsr_ :: KA_Rate rate
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
madsr_ ia idec isl ir idel =
    opcodeStmt1 "madsr" args
  where
    args = [ uniRate ia,    uniRate idec
           , uniRate isl,   uniRate ir
           , uniRate idel ]

xadsr :: KA_Rate rate
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 rate
xadsr ia idec isl ir = 
    opcodeStmt1 "xadsr" args
  where
    args = [ uniRate ia, uniRate idec, uniRate isl, uniRate ir ]


xadsr_ :: KA_Rate rate
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
xadsr_ ia idec isl ir idel =
    opcodeStmt1 "xadsr" args 
  where
    args = [ uniRate ia,    uniRate idec
           , uniRate isl,   uniRate ir
           , uniRate idel ]


mxadsr :: KA_Rate rate
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
mxadsr ia idec isl ir =
    opcodeStmt1 "mxadsr" args 
  where 
    args = [ uniRate ia,   uniRate idec
           , uniRate isl,  uniRate ir ]

mxadsr_ :: KA_Rate rate
        => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
mxadsr_ ia idec isl ir idel = 
    opcodeStmt1 "mxadsr" args 
  where
    args = [ uniRate ia,   uniRate idec
           , uniRate isl,  uniRate ir
           , uniRate idel ]


--------------------------------------------------------------------------------
-- Table access

table :: (Rate rate, Rate r1)
      => Expr r1 -> Int -> Opcode1 rate
table ndx ifn =
    opcodeStmt1 "table" args 
  where
    args = [ uniRate ndx, fromIntegral ifn ]


table_ :: (Rate rate, Rate r1)
       => Expr r1 -> Int
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
table_ ndx ifn ixmode ixoff ixwrap =
    opcodeStmt1 "table" args 
  where
    args = [ uniRate ndx,       fromIntegral ifn
           , uniRate ixmode,    uniRate ixoff
           , uniRate ixwrap ]


tablei :: (Rate rate, Rate r1)
       => Expr r1 -> Int -> Opcode1 rate
tablei ndx ifn =
    opcodeStmt1 "tablei" args 
  where
    args = [ uniRate ndx, fromIntegral ifn ]


tablei_ :: (Rate rate, Rate r1)
        => Expr r1 -> Int
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
tablei_ ndx ifn ixmode ixoff ixwrap =
    opcodeStmt1 "tablei" args 
  where
    args = [ uniRate ndx,       fromIntegral ifn
           , uniRate ixmode,    uniRate ixoff
           , uniRate ixwrap ]



table3 :: (Rate rate, Rate r1)
       => Expr r1 -> Int -> Opcode1 rate
table3 ndx ifn =
    opcodeStmt1 "table" args 
  where
    args = [ uniRate ndx, fromIntegral ifn ]


table3_ :: (Rate rate, Rate r1)
        => Expr r1 -> Int 
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
table3_ ndx ifn ixmode ixoff ixwrap =
    opcodeStmt1 "table3" args 
  where
    args = [ uniRate ndx,       fromIntegral ifn
           , uniRate ixmode,    uniRate ixoff
           , uniRate ixwrap ]


oscil1 :: Expr IInit -> Expr KRate -> Expr IInit -> Int
       -> Opcode1 KRate
oscil1 idel kamp idur ifn =
    opcodeStmt1 "oscil1" args 
  where
    args = [ uniRate idel,  uniRate kamp
           , uniRate idur,  fromIntegral ifn ]


oscil1i :: Expr IInit -> Expr KRate -> Expr IInit -> Int
        -> Opcode1 KRate
oscil1i idel kamp idur ifn =
    opcodeStmt1 "oscil1i" args 
  where
    args = [ uniRate idel,  uniRate kamp
           , uniRate idur,  fromIntegral ifn ]


osciln :: Expr KRate -> Expr IInit -> Int -> Expr IInit 
       -> Opcode1 KRate
osciln kamp ifrq ifn itimes =
    opcodeStmt1 "osciln" args 
  where 
    args = [ uniRate kamp,      uniRate ifrq
           , fromIntegral ifn,  uniRate itimes ]


--------------------------------------------------------------------------------
-- Phasors

-- class CPhasor rate where


phasor :: (KA_Rate rate, Rate r1)
       => Expr r1 -> Opcode1 rate
phasor cps =
    opcodeStmt1 "phasor" args 
  where
    args = [ uniRate cps ]


phasor_ :: (KA_Rate rate, Rate r1)
        => Expr r1 -> Expr IInit -> Opcode1 rate
phasor_ cps iphs =
    opcodeStmt1 "phasor" args 
  where
    args = [ uniRate cps, uniRate iphs ]



phasorbnk :: (KA_Rate rate, Rate r1)
          => Expr r1 -> Expr KRate -> Expr IInit 
          -> Opcode1 rate
phasorbnk cps kindx icnt =
    opcodeStmt1 "phasorbnk" args 
  where
    args = [uniRate cps, uniRate kindx, uniRate icnt ]


phasorbnk_  :: (KA_Rate rate, Rate r1)
            => Expr r1 -> Expr KRate -> Expr IInit -> Expr IInit 
            -> Opcode1 rate
phasorbnk_ cps kindx icnt iphs =
    opcodeStmt1 "phasorbnk" args 
  where
    args = [ uniRate cps,   uniRate kindx
           , uniRate icnt,  uniRate iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

-- | Note for A rate, cps can seemingly be any type.
-- 
oscil :: (KA_Rate rate, Rate r1, Rate r2)
      => Expr r1 -> Expr r2 -> Int -> Opcode1 rate
oscil amp cps ifn = 
    opcodeStmt1 "oscil" args 
  where
    args = [ uniRate amp, uniRate cps, fromIntegral ifn ]


oscil_ :: (KA_Rate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int -> Expr IInit  
       -> Opcode1 rate
oscil_ amp cps ifn iphs =
    opcodeStmt1 "oscil" args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


-- | Prefer @oscil@ rather than this one...
--
oscili :: (KA_Rate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int -> Opcode1 rate
oscili amp cps ifn = 
    opcodeStmt1 "oscili" args 
  where
    args = [ uniRate amp, uniRate cps, fromIntegral ifn ]


oscili_ :: (KA_Rate rate, Rate r1, Rate r2)
        => Expr r1 -> Expr r2 -> Int -> Expr IInit -> Opcode1 rate
oscili_ amp cps ifn iphs =
    opcodeStmt1 "oscili" args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


oscil3 :: (KA_Rate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int -> Opcode1 rate
oscil3 amp cps ifn =
    opcodeStmt1 "oscil3" args
  where
    args = [ uniRate amp, uniRate cps, fromIntegral ifn ]


oscil3_ :: (KA_Rate rate, Rate r1, Rate r2)
        => Expr r1 -> Expr r2 -> Int -> Expr IInit -> Opcode1 rate
oscil3_ amp cps ifn iphs = 
    opcodeStmt1 "oscil3" args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


poscil :: (KA_Rate rate, Rate r1, Rate r2)
       => Expr r1 -> Expr r2 -> Int 
       -> Opcode1 rate
poscil amp cps ifn  = 
    opcodeStmt1 "poscil" args
  where 
    args = [uniRate amp, uniRate cps, fromIntegral ifn ]


poscil_ :: (KA_Rate rate, Rate r1, Rate r2)
        => Expr r1 -> Expr r2 -> Int -> Expr IInit -> Opcode1 rate
poscil_ amp cps ifn iphs = 
    opcodeStmt1 "poscil" args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


poscil3 :: (KA_Rate rate, Rate r1, Rate r2)
        => Expr r1 -> Expr r2 -> Int -> Opcode1 rate
poscil3 amp cps ifn = 
    opcodeStmt1 "poscil3" args 
  where
    args = [ uniRate amp,  uniRate cps, fromIntegral ifn ]


poscil3_ :: (KA_Rate rate, Rate r1, Rate r2)
         => Expr r1 -> Expr r2 -> Int -> Expr IInit -> Opcode1 rate
poscil3_ amp cps ifn iphs = 
    opcodeStmt1 "poscil3" args 
  where
    args = [ uniRate amp,       uniRate cps
           , fromIntegral ifn,  uniRate iphs ]


lfo :: (KA_Rate rate, Rate r1, Rate r2)
    => Expr r1 -> Expr r2 -> Opcode1 rate
lfo amp cps = 
    opcodeStmt1 "lfo" args 
  where
    args = [ uniRate amp, uniRate cps ]


lfo_ :: (KA_Rate rate, Rate r1, Rate r2)
     => Expr r1 -> Expr r2 -> Expr IInit -> Opcode1 rate
lfo_ amp cps itype =
    opcodeStmt1 "lfo" args 
  where
    args = [ uniRate amp, uniRate cps, uniRate itype ]

--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this
-- correct?

buzz :: (Rate r1, Rate r2)
     => Expr r1 -> Expr r2 -> Expr KRate -> Int  
     -> Opcode1 ARate
buzz xamp xcps knh ifn =
    opcodeStmt1 "buzz" args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   fromIntegral ifn ]


buzz_ :: (Rate r1, Rate r2)
      => Expr r1 -> Expr r2 -> Expr KRate -> Int -> Expr IInit 
      -> Opcode1 ARate
buzz_ xamp xcps knh ifn iphs =
    opcodeStmt1 "buzz" args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   fromIntegral ifn
           , uniRate iphs ]


gbuzz :: (Rate r1, Rate r2)
      => Expr r1 -> Expr r2 
      -> Expr KRate -> Expr KRate -> Expr KRate -> Int
      -> Opcode1 ARate
gbuzz xamp xcps knh klh kr ifn =
    opcodeStmt1 "gbuzz" args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   uniRate klh
           , uniRate kr,    fromIntegral ifn ]


gbuzz_ :: (Rate r1, Rate r2)
       => Expr r1 -> Expr r2 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Int -> Expr IInit
       -> Opcode1 ARate
gbuzz_ xamp xcps knh klh kr ifn iphs =
    opcodeStmt1 "gbuzz" args 
  where
    args = [ uniRate xamp,  uniRate xcps
           , uniRate knh,   uniRate klh
           , uniRate kr,    fromIntegral ifn
           , uniRate iphs ]

vco :: Expr KRate -> Expr KRate -> Expr IInit -> Expr KRate 
    -> Int -> Expr IInit 
    -> Opcode1 ARate
vco kamp kfqc iwave kpw ifn imaxd =
    opcodeStmt1 "gbuzz" args 
  where
    args = [ uniRate kamp,      uniRate kfqc
           , uniRate iwave,     uniRate kpw
           , fromIntegral ifn,  uniRate imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Expr KRate -> Expr KRate -> Expr KRate -> String
      -> Opcode1 ARate
adsyn kamod kfmod ksmod ss =
    opcodeStmt1 "adsyn" args 
  where
    args = [ uniRate kamod, uniRate kfmod
           , uniRate ksmod, StringE ss ]


adsynt :: Expr KRate -> Expr KRate 
       -> Int -> Int -> Int 
       -> Expr IInit  
       -> Opcode1 ARate
adsynt kamp kcps iwfn ifreqfn iampfn icnt =
    opcodeStmt1 "adsynt" args 
  where
    args = [ uniRate kamp,        uniRate kcps
           , fromIntegral iwfn,   fromIntegral ifreqfn
           , fromIntegral iampfn, uniRate icnt ]


adsynt_ :: Expr KRate -> Expr KRate 
        -> Int -> Int -> Int 
        -> Expr IInit -> Expr IInit
        -> Opcode1 ARate
adsynt_ kamp kcps iwfn ifreqfn iampfn icnt iphs =
    opcodeStmt1 "adsynt" args 
  where
    args = [ uniRate kamp,        uniRate kcps
           , fromIntegral iwfn,   fromIntegral ifreqfn
           , fromIntegral iampfn, uniRate icnt
           , uniRate iphs ]


hsboscil :: Expr KRate -> Expr KRate -> Expr KRate -> Expr IInit
         -> Int -> Int 
         -> Opcode1 ARate
hsboscil kamp ktone kbrite ibasfreq iwfn ioctfn = 
    opcodeStmt1 "hsboscil" args 
  where
    args = [ uniRate kamp,      uniRate ktone
           , uniRate kbrite,    uniRate ibasfreq
           , fromIntegral iwfn, fromIntegral ioctfn ]


hsboscil_ :: Expr KRate -> Expr KRate -> Expr KRate -> Expr IInit
          -> Int -> Int -> Expr IInit
          -> Opcode1 ARate
hsboscil_ kamp ktone kbrite ibasfreq iwfn ioctfn iphs = 
    opcodeStmt1 "hsboscil" args 
  where
    args = [ uniRate kamp,      uniRate ktone
           , uniRate kbrite,    uniRate ibasfreq
           , fromIntegral iwfn, fromIntegral ioctfn
           , uniRate iphs ]


--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Rate r1
       => Expr r1 -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate -> Int 
       -> Opcode1 ARate
foscil xamp kcps kcar kmod kndx ifn =
    opcodeStmt1 "foscil" args 
  where
    args = [ uniRate xamp,    uniRate kcps
           , uniRate kcar,    uniRate kmod
           , uniRate kndx,    fromIntegral ifn ]


foscil_ :: Rate r1
        => Expr r1 -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Int -> Expr IInit 
        -> Opcode1 ARate
foscil_ xamp kcps kcar kmod kndx ifn iphs =
    opcodeStmt1 "foscil" args 
  where
    args = [ uniRate xamp,    uniRate kcps
           , uniRate kcar,    uniRate kmod
           , uniRate kndx,    fromIntegral ifn
           , uniRate iphs ]

foscili :: Rate r1 
        => Expr r1 -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Int
        -> Opcode1 ARate
foscili xamp kcps kcar kmod kndx ifn =
    opcodeStmt1 "foscili" args 
  where
    args = [ uniRate xamp,    uniRate kcps
           , uniRate kcar,    uniRate kmod
           , uniRate kndx,    fromIntegral ifn ]


foscili_ :: Rate r1 
         => Expr r1 -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
         -> Int -> Expr IInit
         -> Opcode1 ARate
foscili_ xamp kcps kcar kmod kndx ifn iphs =
    opcodeStmt1 "foscil" args 
  where
    args = [ uniRate xamp,  uniRate kcps
           , uniRate kcar,  uniRate kmod
           , uniRate kndx,  fromIntegral ifn
           , uniRate iphs ]


fmvoice :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Int -> Int -> Int -> Int -> Int 
        -> Opcode1 ARate
fmvoice kamp kfreq kvowel ktilt kvibamt kvibrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcodeStmt1 "fmvoice" args 
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
       -> Opcode1 ARate
fmbell kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmbell" args 
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
        -> Opcode1 ARate
fmrhode kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmrhode" args 
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
         -> Opcode1 ARate
fmwurlie kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmwurlie" args 
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
        -> Opcode1 ARate
fmmetal kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmmetal" args 
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
     -> Opcode1 ARate
fmb3 kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    opcodeStmt1 "fmb3" args 
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
         -> Opcode1 ARate
fmpercfl kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcodeStmt1 "fmpercfl" args 
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
loscil :: Rate r1 
       => Expr r1 -> Expr KRate -> Int -> Expr IInit -> Expr IInit
       -> Opcode1 ARate
loscil xamp kcps ifn ibase imod1 =
    opcodeStmt1 "loscil" args
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate imod1 ]


-- | Stereo version of 'loscil'.
--
-- Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil :: Rate r1
         => Expr r1 -> Expr KRate -> Int -> Expr IInit -> Expr IInit
         -> Opcode2 ARate
biloscil xamp kcps ifn ibase imod1 =
    opcodeStmt2 "loscil" args 
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate imod1 ]


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil3 :: Rate r1 
        => Expr r1 -> Expr KRate -> Int -> Expr IInit -> Expr IInit 
        -> Opcode1 ARate
loscil3 xamp kcps ifn ibase imod1 =
    opcodeStmt1 "loscil3" args 
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate imod1 ]


-- | Stereo version of 'loscil3'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil3 :: Rate r1
          => Expr r1 -> Expr KRate -> Int
          -> Expr IInit -> Expr IInit 
          -> Opcode2 ARate
biloscil3 xamp kcps ifn ibase imod1 =
    opcodeStmt2 "loscil3" args 
  where
    args = [ uniRate xamp,      uniRate kcps
           , fromIntegral ifn,  uniRate ibase
           , uniRate  imod1 ]



lposcil :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate -> Int
        -> Opcode1 ARate
lposcil kamp kfreqrat kloop kend ifn =
    opcodeStmt1 "lposcil" args 
  where
    args = [ uniRate kamp,    uniRate kfreqrat
           , uniRate kloop,   uniRate kend
           , fromIntegral ifn ]


lposcil3 :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate -> Int
         -> Opcode1 ARate
lposcil3 kamp kfreqrat kloop kend ifn =
    opcodeStmt1 "lposcil3" args 
  where
    args = [ uniRate kamp,     uniRate kfreqrat
           , uniRate kloop,    uniRate kend
           , fromIntegral ifn ]



--------------------------------------------------------------------------------
-- Models and emulations

moog :: Expr KRate -> Expr KRate -> Expr KRate 
     -> Expr KRate -> Expr KRate -> Expr KRate
     -> Int -> Int -> Int
     -> Opcode1 ARate
moog kamp kfreq kfiltq kfiltrate kvibf kvamp iafn iwfn ivfn =
    opcodeStmt1 "moog" args
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
       -> Opcode1 ARate
shaker kamp kfreq kbeans kdamp ktimes idecay =
    opcodeStmt1 "shaker" args 
  where
    args = [ uniRate kamp,     uniRate kfreq
           , uniRate kbeans,   uniRate kdamp
           , uniRate ktimes,   uniRate idecay ]


marimba :: Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr IInit 
        -> Int -> Expr KRate 
        -> Expr KRate -> Int
        -> Expr IInit
        -> Opcode1 ARate
marimba kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec =
    opcodeStmt1 "marimba" args 
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
      -> Opcode1 ARate
vibes kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec =
    opcodeStmt1 "vibes" args 
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
       -> Opcode1 ARate
mandol kamp kfreq kpluck kdetune kgain ksize ifn iminfreq =
    opcodeStmt1 "marimba" args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kpluck,    uniRate kdetune
           , uniRate kgain,     uniRate ksize
           , fromIntegral ifn,  uniRate iminfreq ]


gogobel :: Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Expr KRate -> Expr KRate -> Int
        -> Opcode1 ARate
gogobel kamp kfreq ihrd ipos imp kvibf kvamp ivibfn  =
    opcodeStmt1 "gogobel" args 
  where
    args = [ uniRate kamp,     uniRate kfreq
           , uniRate ihrd,     uniRate ipos
           , uniRate imp,      uniRate kvibf
           , uniRate kvamp,    fromIntegral ivibfn ]


voice :: Expr KRate -> Expr KRate -> Expr KRate 
      -> Expr KRate -> Expr KRate -> Expr KRate
      -> Int -> Int 
      -> Opcode1 ARate
voice kamp kfreq kphoneme kform kvibf kvamp ifn ivfn =
    opcodeStmt1 "voice" args 
  where
    args = [ uniRate kamp,      uniRate kfreq
           , uniRate kphoneme,  uniRate kform
           , uniRate kvibf,     uniRate kvamp
           , fromIntegral ifn,  fromIntegral ivfn ]


lorenz :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit
       -> Opcode3 ARate
lorenz ks kr kb kh ix iy iz ivx =
    opcodeStmt3 "lorenz" args
  where
    args = [ uniRate ks, uniRate kr
           , uniRate kb, uniRate kh
           , uniRate ix, uniRate iy
           , uniRate iz, uniRate ivx ]


planet :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr IInit -> Expr IInit
       -> Opcode3 ARate
planet kmass1 kmass2 ksep ix iy iz ivx ivy ivz idelta ifriction =
    opcodeStmt3 "planet" args 
  where
    args = [uniRate kmass1,   uniRate kmass2
           , uniRate ksep,     uniRate ix
           , uniRate iy,       uniRate iz
           , uniRate ivx,      uniRate ivy
           , uniRate ivz,      uniRate idelta
           , uniRate ifriction ]

--------------------------------------------------------------------------------
-- Random noise generators



rand :: KA_Rate rate => Expr rate -> Opcode1 rate
rand amp =
    opcodeStmt1 "rand" args 
  where
    args = [ uniRate amp ]


randh :: (KA_Rate rate, Rate r1, Rate r2) 
      => Expr r1 -> Expr r2 -> Opcode1 rate
randh amp cps =
    opcodeStmt1 "rand" args 
  where
    args = [uniRate amp, uniRate cps ]


randi :: (KA_Rate rate, Rate r1, Rate r2)
      => Expr r1 -> Expr r2 -> Opcode1 rate
randi amp cps =
    opcodeStmt1 "rand" args 
  where
    args = [ uniRate amp, uniRate cps ]


linrand :: Rate rate
        => Expr KRate -> Opcode1 rate
linrand krange =
    opcodeStmt1 "linrand" args 
  where
    args = [uniRate krange ]


trirand :: Rate rate 
        => Expr KRate -> Opcode1 rate
trirand krange =
    opcodeStmt1 "trirand" args 
  where
    args = [ uniRate krange ]


exprand :: Rate rate
        => Expr KRate -> Opcode1 rate
exprand krange =
    opcodeStmt1 "exprand" args 
  where
    args = [ uniRate krange ]


bexprand :: Rate rate 
         => Expr KRate -> Opcode1 rate
bexprand krange =
    opcodeStmt1 "bexprand" args 
  where
    args = [ uniRate krange ]


cauchy :: Rate rate
       => Expr KRate -> Opcode1 rate
cauchy kalpha =
    opcodeStmt1 "cauchy" args 
  where
    args = [ uniRate kalpha ]


pcauchy :: Rate rate
        => Expr KRate -> Opcode1 rate
pcauchy kalpha =
    opcodeStmt1 "pcauchy" args 
  where
    args = [ uniRate kalpha ]

poisson :: Rate rate 
        => Expr KRate -> Opcode1 rate
poisson klambda =
    opcodeStmt1 "poisson" args 
  where
    args = [ uniRate klambda ]


gauss :: Rate rate
      => Expr KRate -> Opcode1 rate
gauss krange =
    opcodeStmt1 "gauss" args 
  where
   args = [ uniRate krange ]


weibull :: Rate rate
        => Expr KRate -> Expr KRate -> Opcode1 rate
weibull ksigma ktau =
    opcodeStmt1 "weibull" args 
  where
    args = [ uniRate ksigma, uniRate ktau ]


betarand :: Rate rate 
         => Expr KRate -> Expr KRate -> Expr KRate  -> Opcode1 rate
betarand krange kalpha kbeta =
    opcodeStmt1 "betarand" args
  where
    args = [uniRate krange,  uniRate kalpha, uniRate kbeta ]


unirand :: Rate rate
        => Expr KRate -> Opcode1 rate
unirand krange =
    opcodeStmt1 "unirand" args 
  where
    args = [ uniRate krange ]


