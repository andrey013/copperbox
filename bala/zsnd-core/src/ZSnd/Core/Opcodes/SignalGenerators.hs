{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.SignalGenerators
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Signal generating opcodes.
-- 
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.SignalGenerators
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


import ZSnd.Core.CsoundInst.Typed

--------------------------------------------------------------------------------
-- Linear and Exponential generators





line :: forall rate . (KA_Rate rate)
     => Expr IInit -> Expr IInit -> Expr IInit -> Opcode1 rate
line ia idur ib = 
    Opcode1 "line" [ getExprI ia, getExprI idur, getExprI ib ] 
    



expon :: forall rate. (KA_Rate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Opcode1 rate
expon ia idur ib = 
    Opcode1 "expon" [getExprI ia, getExprI idur, getExprI ib ]



linseg :: forall rate. (KA_Rate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> [(Expr IInit, Expr IInit)]  
       -> Opcode1 rate
linseg ia idur ib xs = 
    Opcode1 "linseg" (getExprI ia : getExprI idur : getExprI ib : rest)
  where
    rest = concatMap (\(a,b) -> [getExprI a, getExprI b]) xs
     
    



linsegr :: forall rate. (KA_Rate rate)
        => Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Expr IInit ->  Expr IInit 
        -> Opcode1 rate
linsegr ia idur ib xs irel iz =
    Opcode1 "linsegr" (getExprI ia : getExprI idur : getExprI ib : rest ++ end)
  where
    rest = concatMap (\(a,b) -> [getExprI a, getExprI b]) xs
    end  = [getExprI irel, getExprI iz]
     


expseg :: forall rate. (KA_Rate rate )
       => Expr IInit -> Expr IInit -> Expr IInit -> [(Expr IInit, Expr IInit)]
       -> Opcode1 rate
expseg ia idur ib xs =
    Opcode1 "expseg" (getExprI ia : getExprI idur : getExprI ib : rest)
  where
    rest = concatMap (\(a,b) -> [getExprI a, getExprI b]) xs
     



expsegr :: forall rate. (KA_Rate rate)
        => Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
expsegr ia idur ib xs irel iz = 
    Opcode1 "linsegr" (getExprI ia : getExprI idur : getExprI ib : rest ++ end)
  where
    rest = concatMap (\(a,b) -> [getExprI a, getExprI b]) xs
    end  = [getExprI irel, getExprI iz]
     




expsega :: Expr IInit -> Expr IInit -> Expr IInit
        -> [(Expr IInit, Expr IInit)]
        -> Opcode1 ARate
expsega ia idur ib xs =
    Opcode1 "expsega" (getExprI ia : getExprI idur : getExprI ib : rest)
  where
    rest = concatMap (\(a,b) -> [getExprI a, getExprI b]) xs
     


adsr :: forall rate. (KA_Rate rate)
     => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
     -> Opcode1 rate
adsr ia idec isl ir = 
    Opcode1 "adsr" [ getExprI ia,   getExprI idec, getExprI isl,  getExprI ir ]


adsr_ :: forall rate. (KA_Rate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit
      -> Opcode1 rate
adsr_ ia idec isl ir idel =
    Opcode1 "adsr" [ getExprI ia,   getExprI idec
                   , getExprI isl,  getExprI ir
                   , getExprI idel ]


madsr :: forall rate. (KA_Rate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 rate
madsr ia idec isl ir =
    Opcode1 "madsr" [ getExprI ia,    getExprI idec
                    , getExprI isl,   getExprI ir ]


madsr_ :: forall rate. (KA_Rate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
madsr_ ia idec isl ir idel =
    Opcode1 "madsr" [ getExprI ia,    getExprI idec
                    , getExprI isl,   getExprI ir
                    , getExprI idel ]

xadsr :: forall rate. (KA_Rate rate)
      => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
      -> Opcode1 rate
xadsr ia idec isl ir = 
    Opcode1 "xadsr" [ getExprI ia,    getExprI idec
                    , getExprI isl,   getExprI ir ]


xadsr_ :: forall rate. (KA_Rate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
xadsr_ ia idec isl ir idel =
    Opcode1 "xadsr" [ getExprI ia,    getExprI idec
                    , getExprI isl,   getExprI ir
                    , getExprI idel ]


mxadsr :: forall rate. (KA_Rate rate)
       => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
mxadsr ia idec isl ir =
    Opcode1 "mxadsr" [ getExprI ia,   getExprI idec
                     , getExprI isl,  getExprI ir ]

mxadsr_ :: forall rate. (KA_Rate rate)
        => Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
mxadsr_ ia idec isl ir idel = 
    Opcode1 "mxadsr" [ getExprI ia,   getExprI idec
                     , getExprI isl,  getExprI ir
                     , getExprI idel ]


--------------------------------------------------------------------------------
-- Table access

table :: forall rate1 rate. (Rate rate)
      => Expr rate1 -> Expr ITableNum -> Opcode1 rate
table ndx ifn =
    Opcode1 "table" [ getExprUniv ndx, getExprUniv ifn ]


table_ :: forall rate1 rate. (Rate rate)
       => Expr rate1 -> Expr ITableNum 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
table_ ndx ifn ixmode ixoff ixwrap =
    Opcode1 "table" [ getExprUniv ndx,    getExprUniv ifn
                    , getExprI ixmode,    getExprI ixoff
                    , getExprI ixwrap ]


tablei :: forall rate1 rate. (Rate rate)
       => Expr rate1 -> Expr ITableNum -> Opcode1 rate
tablei ndx ifn =
    Opcode1 "tablei" [ getExprUniv ndx, getExprUniv ifn ]


tablei_ :: forall rate1 rate. (Rate rate)
       => Expr rate1 -> Expr ITableNum 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Opcode1 rate
tablei_ ndx ifn ixmode ixoff ixwrap =
    Opcode1 "tablei" [ getExprUniv ndx,   getExprUniv ifn
                     , getExprI ixmode,   getExprI ixoff
                     , getExprI ixwrap ]



table3 :: forall rate1 rate. (Rate rate)
       => Expr rate1 -> Expr ITableNum -> Opcode1 rate
table3 ndx ifn =
    Opcode1 "table" [ getExprUniv ndx, getExprUniv ifn ]


table3_ :: forall rate1 rate. (Rate rate)
        => Expr rate1 -> Expr ITableNum 
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 rate
table3_ ndx ifn ixmode ixoff ixwrap =
    Opcode1 "table3" [ getExprUniv ndx,   getExprUniv ifn
                     , getExprI ixmode,   getExprI ixoff
                     , getExprI ixwrap ]


oscil1 :: Expr IInit -> Expr KRate -> Expr IInit -> Expr ITableNum 
       -> Opcode1 KRate
oscil1 idel kamp idur ifn =
    Opcode1 "oscil1" [ getExprI idel,   getExprK kamp
                     , getExprI idur,   getExprUniv ifn ]


oscil1i :: Expr IInit -> Expr KRate -> Expr IInit -> Expr ITableNum
        -> Opcode1 KRate
oscil1i idel kamp idur ifn =
    Opcode1 "oscil1i" [ getExprI idel,  getExprK kamp
                      , getExprI idur,  getExprUniv ifn ]


osciln :: Expr KRate -> Expr IInit -> Expr ITableNum -> Expr IInit 
       -> Opcode1 KRate
osciln kamp ifrq ifn itimes =
    Opcode1 "osciln" [ getExprK kamp,            getExprI ifrq
                     , getExprUniv ifn,   getExprI itimes ]


--------------------------------------------------------------------------------
-- Phasors

-- class CPhasor rate where


phasor :: forall rate1 rate. (KA_Rate rate)
       => Expr rate1 -> Opcode1 rate
phasor cps =
    Opcode1 "phasor" [ getExprUniv cps ]


phasor_ :: forall rate1 rate. (KA_Rate rate)
        => Expr rate1 -> Expr IInit -> Opcode1 rate
phasor_ cps iphs =
    Opcode1 "phasor" [ getExprUniv cps, getExprI iphs ]


phasorbnk :: forall rate1 rate. (KA_Rate rate)
          => Expr rate1 -> Expr KRate -> Expr IInit 
          -> Opcode1 rate
phasorbnk cps kindx icnt =
    Opcode1 "phasorbnk" [ getExprUniv cps,  getExprK kindx
                        , getExprI icnt ]


phasorbnk_  :: forall rate1 rate. (KA_Rate rate)
            => Expr rate1 -> Expr KRate -> Expr IInit -> Expr IInit 
            -> Opcode1 rate
phasorbnk_ cps kindx icnt iphs =
    Opcode1 "phasorbnk" [ getExprUniv cps,  getExprK kindx
                        , getExprI icnt,    getExprI iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

-- | Note for A rate, cps can seemingly be any type.
-- 
oscil :: forall r1 r2 rate. (KA_Rate rate)
      => Expr r1 -> Expr r2 -> Expr ITableNum -> Opcode1 rate
oscil amp cps ifn = 
    Opcode1 "oscil" [ getExprUniv amp,  getExprUniv cps
                    , getExprUniv ifn ]


oscil_ :: forall r1 r2 rate. (KA_Rate rate)
       => Expr r1 -> Expr r2 -> Expr ITableNum -> Expr IInit  
       -> Opcode1 rate
oscil_ amp cps ifn iphs =
    Opcode1 "oscil" [ getExprUniv amp,  getExprUniv cps
                    , getExprUniv ifn,  getExprI iphs ]


-- | Prefer @oscil@ rather than this one...
--
oscili :: forall r1 r2 rate. (KA_Rate rate)
       => Expr r1 -> Expr r2 -> Expr ITableNum 
       -> Opcode1 rate
oscili amp cps ifn = 
    Opcode1 "oscili" [ getExprUniv amp, getExprUniv cps
                     , getExprUniv ifn ]


oscili_ :: forall r1 r2 rate. (KA_Rate rate)
        => Expr r1 -> Expr r2 -> Expr ITableNum -> Expr IInit 
        -> Opcode1 rate
oscili_ amp cps ifn iphs =
    Opcode1 "oscili" [ getExprUniv amp,   getExprUniv cps
                     , getExprUniv ifn,   getExprI iphs ]


oscil3 :: forall r1 r2 rate. (KA_Rate rate)
       => Expr r1 -> Expr r2 -> Expr ITableNum 
       -> Opcode1 rate
oscil3 amp cps ifn =
    Opcode1 "oscil3" [ getExprUniv amp, getExprUniv cps
                     , getExprUniv ifn ]


oscil3_ :: forall r1 r2 rate. (KA_Rate rate)
        => Expr r1 -> Expr r2 -> Expr ITableNum -> Expr IInit 
        -> Opcode1 rate
oscil3_ amp cps ifn iphs = 
    Opcode1 "oscil3" [ getExprUniv amp,   getExprUniv cps
                     , getExprUniv ifn,   getExprI iphs ]


poscil :: forall r1 r2 rate. (KA_Rate rate)
       => Expr r1 -> Expr r2 -> Expr ITableNum 
       -> Opcode1 rate
poscil amp cps ifn  = 
    Opcode1 "poscil" [ getExprUniv amp, getExprUniv cps
                     , getExprUniv ifn ]


poscil_ :: forall r1 r2 rate. (KA_Rate rate)
        => Expr r1 -> Expr r2 -> Expr ITableNum -> Expr IInit 
        -> Opcode1 rate
poscil_ amp cps ifn iphs = 
    Opcode1 "poscil" [ getExprUniv amp,   getExprUniv cps
                     , getExprUniv ifn,   getExprI iphs ]


poscil3 :: forall r1 r2 rate. (KA_Rate rate)
        => Expr r1 -> Expr r2 -> Expr ITableNum
        -> Opcode1 rate
poscil3 amp cps ifn = 
    Opcode1 "poscil3" [ getExprUniv amp,  getExprUniv cps
                      , getExprUniv ifn ]


poscil3_  :: forall r1 r2 rate. (KA_Rate rate)
          => Expr r1 -> Expr r2 -> Expr ITableNum -> Expr IInit 
          -> Opcode1 rate
poscil3_ amp cps ifn iphs = 
    Opcode1 "poscil3" [ getExprUniv amp,  getExprUniv cps
                      , getExprUniv ifn,  getExprI iphs ]


lfo :: forall r1 r2 rate. (KA_Rate rate)
    => Expr r1 -> Expr r2 -> Opcode1 rate
lfo amp cps = 
    Opcode1 "lfo" [ getExprUniv amp,  getExprUniv cps]


lfo_ :: forall r1 r2 rate. (KA_Rate rate)
     => Expr r1 -> Expr r2 -> Expr IInit 
     -> Opcode1 rate
lfo_ amp cps itype =
    Opcode1 "lfo" [ getExprUniv amp,  getExprUniv cps
                  , getExprI itype ]

--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this
-- correct?

buzz :: Expr r1 -> Expr r2 -> Expr KRate -> Expr ITableNum  
     -> Opcode1 ARate
buzz xamp xcps knh ifn =
    Opcode1 "buzz" [ getExprUniv xamp,  getExprUniv xcps
                   , getExprK knh,      getExprUniv ifn ]


buzz_ :: Expr r1 -> Expr r2 -> Expr KRate -> Expr ITableNum -> Expr IInit 
      -> Opcode1 ARate
buzz_ xamp xcps knh ifn iphs =
    Opcode1 "buzz" [ getExprUniv xamp,  getExprUniv xcps
                   , getExprK knh,      getExprUniv ifn
                   , getExprI iphs ]


gbuzz :: Expr r1 -> Expr r2 
      -> Expr KRate -> Expr KRate -> Expr KRate -> Expr ITableNum
      -> Opcode1 ARate
gbuzz xamp xcps knh klh kr ifn =
    Opcode1 "gbuzz" [ getExprUniv xamp,   getExprUniv xcps
                    , getExprK knh,       getExprK klh
                    , getExprK kr,        getExprUniv ifn ]


gbuzz_ :: Expr r1 -> Expr r2 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr ITableNum -> Expr IInit
       -> Opcode1 ARate
gbuzz_ xamp xcps knh klh kr ifn iphs =
    Opcode1 "gbuzz" [ getExprUniv xamp,   getExprUniv xcps
                    , getExprK knh,       getExprK klh
                    , getExprK kr,        getExprUniv ifn
                    , getExprI iphs ]

vco :: Expr KRate -> Expr KRate -> Expr IInit -> Expr KRate 
    -> Expr ITableNum -> Expr IInit 
    -> Opcode1 ARate
vco kamp kfqc iwave kpw ifn imaxd =
    Opcode1 "gbuzz" [ getExprK kamp,    getExprK kfqc
                    , getExprI iwave,   getExprK kpw
                    , getExprUniv ifn,  getExprI imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Expr KRate -> Expr KRate -> Expr KRate -> String
      -> Opcode1 ARate
adsyn kamod kfmod ksmod ss =
    Opcode1 "adsyn" [ getExprK kamod,   getExprK kfmod
                    , getExprK ksmod,   getExprI $ filecode ss ]


adsynt :: Expr KRate -> Expr KRate 
       -> Expr ITableNum -> Expr ITableNum -> Expr ITableNum 
       -> Expr IInit  
       -> Opcode1 ARate
adsynt kamp kcps iwfn ifreqfn iampfn icnt =
    Opcode1 "adsynt" [ getExprK kamp,       getExprK kcps
                     , getExprUniv iwfn,    getExprUniv ifreqfn
                     , getExprUniv iampfn,  getExprI icnt ]


adsynt_ :: Expr KRate -> Expr KRate 
       -> Expr ITableNum -> Expr ITableNum -> Expr ITableNum 
       -> Expr IInit -> Expr IInit
       -> Opcode1 ARate
adsynt_ kamp kcps iwfn ifreqfn iampfn icnt iphs =
    Opcode1 "adsynt" [ getExprK kamp,       getExprK kcps
                     , getExprUniv iwfn,    getExprUniv ifreqfn
                     , getExprUniv iampfn,  getExprI icnt
                     , getExprI iphs ]


hsboscil :: Expr KRate -> Expr KRate -> Expr KRate -> Expr IInit
         -> Expr ITableNum -> Expr ITableNum 
         -> Opcode1 ARate
hsboscil kamp ktone kbrite ibasfreq iwfn ioctfn = 
    Opcode1 "hsboscil" [ getExprK kamp,     getExprK ktone
                       , getExprK kbrite,   getExprI ibasfreq
                       , getExprUniv iwfn,  getExprUniv ioctfn ]


hsboscil_ :: Expr KRate -> Expr KRate -> Expr KRate -> Expr IInit
          -> Expr ITableNum -> Expr ITableNum -> Expr IInit
          -> Opcode1 ARate
hsboscil_ kamp ktone kbrite ibasfreq iwfn ioctfn iphs = 
    Opcode1 "hsboscil" [ getExprK kamp,     getExprK ktone
                       , getExprK kbrite,   getExprI ibasfreq
                       , getExprUniv iwfn,  getExprUniv ioctfn
                       , getExprI iphs ]


--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr ITableNum 
       -> Opcode1 ARate
foscil xamp kcps kcar kmod kndx ifn =
    Opcode1 "foscil" [ getExprUniv xamp,  getExprK kcps
                     , getExprK kcar,     getExprK kmod
                     , getExprK kndx,     getExprUniv ifn ]


foscil_ :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr ITableNum -> Expr IInit 
        -> Opcode1 ARate
foscil_ xamp kcps kcar kmod kndx ifn iphs =
    Opcode1 "foscil" [ getExprUniv xamp,  getExprK kcps
                     , getExprK kcar,     getExprK kmod
                     , getExprK kndx,     getExprUniv ifn
                     , getExprI iphs ]

foscili :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr ITableNum
        -> Opcode1 ARate
foscili xamp kcps kcar kmod kndx ifn =
    Opcode1 "foscili" [ getExprUniv xamp, getExprK kcps
                      , getExprK kcar,    getExprK kmod
                      , getExprK kndx,    getExprUniv ifn ]


foscili_ :: Expr rate -> Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
         -> Expr ITableNum -> Expr IInit
         -> Opcode1 ARate
foscili_ xamp kcps kcar kmod kndx ifn iphs =
    Opcode1 "foscil" [ getExprUniv xamp,  getExprK kcps
                     , getExprK kcar,     getExprK kmod
                     , getExprK kndx,     getExprUniv ifn
                     , getExprI iphs ]


fmvoice :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Expr IInit -> Expr IInit -> Expr IInit
        -> Expr IInit -> Expr IInit
        -> Opcode1 ARate
fmvoice kamp kfreq kvowel ktilt kvibamt kvibrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    Opcode1 "fmvoice" [ getExprK kamp,      getExprK kfreq
                      , getExprK kvowel,    getExprK ktilt
                      , getExprK kvibamt,   getExprK kvibrate
                      , getExprUniv ifn1,   getExprUniv ifn2
                      , getExprUniv ifn3,   getExprUniv ifn4
                      , getExprUniv ivibfn ]


fmbell :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate
       -> Expr IInit -> Expr IInit -> Expr IInit
       -> Expr IInit -> Expr IInit
       -> Opcode1 ARate
fmbell kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    Opcode1 "fmbell" [ getExprK kamp,     getExprK kfreq
                     , getExprK kc1,      getExprK kc2
                     , getExprK kvdepth,  getExprK kvrate
                     , getExprUniv ifn1,  getExprUniv ifn2
                     , getExprUniv ifn3,  getExprUniv ifn4
                     , getExprUniv ivibfn ]


fmrhode :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Expr IInit -> Expr IInit -> Expr IInit
        -> Expr IInit -> Expr IInit
        -> Opcode1 ARate
fmrhode kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    Opcode1 "fmrhode" [ getExprK kamp,      getExprK kfreq
                      , getExprK kc1,       getExprK kc2
                      , getExprK kvdepth,   getExprK kvrate
                      , getExprUniv ifn1,   getExprUniv ifn2
                      , getExprUniv ifn3,   getExprUniv ifn4
                      , getExprUniv ivibfn ]


fmwurlie :: Expr KRate -> Expr KRate -> Expr KRate 
         -> Expr KRate -> Expr KRate -> Expr KRate
         -> Expr IInit -> Expr IInit -> Expr IInit
         -> Expr IInit -> Expr IInit
         -> Opcode1 ARate
fmwurlie kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    Opcode1 "fmwurlie" [ getExprK kamp,     getExprK kfreq
                       , getExprK kc1,      getExprK kc2
                       , getExprK kvdepth,  getExprK kvrate
                       , getExprUniv ifn1,  getExprUniv ifn2
                       , getExprUniv ifn3,  getExprUniv ifn4
                       , getExprUniv ivibfn ]


fmmetal :: Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr KRate -> Expr KRate -> Expr KRate
        -> Expr IInit -> Expr IInit -> Expr IInit
        -> Expr IInit -> Expr IInit
        -> Opcode1 ARate
fmmetal kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    Opcode1 "fmmetal" [ getExprK kamp,     getExprK kfreq
                      , getExprK kc1,      getExprK kc2
                      , getExprK kvdepth,  getExprK kvrate
                      , getExprUniv ifn1,  getExprUniv ifn2
                      , getExprUniv ifn3,  getExprUniv ifn4
                      , getExprUniv ivibfn ]


fmb3 :: Expr KRate -> Expr KRate -> Expr KRate 
     -> Expr KRate -> Expr KRate -> Expr KRate
     -> Expr IInit -> Expr IInit -> Expr IInit
     -> Expr IInit -> Expr IInit
     -> Opcode1 ARate
fmb3 kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn =
    Opcode1 "fmb3" [ getExprK kamp,     getExprK kfreq
                   , getExprK kc1,      getExprK kc2
                   , getExprK kvdepth,  getExprK kvrate
                   , getExprUniv ifn1,  getExprUniv ifn2
                   , getExprUniv ifn3,  getExprUniv ifn4
                   , getExprUniv ivibfn ]


fmpercfl :: Expr KRate -> Expr KRate -> Expr KRate 
         -> Expr KRate -> Expr KRate -> Expr KRate        
         -> Expr IInit -> Expr IInit -> Expr IInit
         -> Expr IInit -> Expr IInit
         -> Opcode1 ARate
fmpercfl kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    Opcode1 "fmpercfl" [ getExprK kamp,     getExprK kfreq
                       , getExprK kc1,      getExprK kc2
                       , getExprK kvdepth,  getExprK kvrate
                       , getExprUniv ifn1,  getExprUniv ifn2
                       , getExprUniv ifn3,  getExprUniv ifn4
                       , getExprUniv ivibfn ]

--------------------------------------------------------------------------------
-- Sample playback

-- Note - it seems idiomatic to want a stereo version will only
-- the mandatory args...


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil :: Expr rate -> Expr KRate -> Expr ITableNum 
       -> Expr IInit -> Expr IInit
       -> Opcode1 ARate
loscil xamp kcps ifn ibase imod1 =
    Opcode1 "loscil" [ getExprUniv xamp,  getExprK kcps
                     , getExprUniv ifn,   getExprI ibase
                     , getExprI imod1 ]


-- | Stereo version of 'loscil'.
--
-- Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil :: Expr rate -> Expr KRate -> Expr ITableNum 
         -> Expr IInit -> Expr IInit
         -> Opcode2 ARate
biloscil xamp kcps ifn ibase imod1 =
    Opcode2 "loscil" [ getExprUniv xamp,  getExprK kcps
                     , getExprUniv ifn,   getExprI ibase
                     , getExprI imod1 ]


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil3 :: Expr rate -> Expr KRate -> Expr ITableNum 
        -> Expr IInit -> Expr IInit 
        -> Opcode1 ARate
loscil3 xamp kcps ifn ibase imod1 =
    Opcode1 "loscil3" [ getExprUniv xamp,  getExprK kcps
                      , getExprUniv ifn,   getExprI ibase
                      , getExprI imod1 ]


-- | Stereo version of 'loscil3'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil3 :: Expr rate -> Expr KRate -> Expr ITableNum 
          -> Expr IInit -> Expr IInit 
          -> Opcode2 ARate
biloscil3 xamp kcps ifn ibase imod1 =
    Opcode2 "loscil3" [ getExprUniv xamp,   getExprK kcps
                      , getExprUniv ifn,    getExprI ibase
                      , getExprI  imod1 ]


lposcil :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
        -> Expr ITableNum
        -> Opcode1 ARate
lposcil kamp kfreqrat kloop kend ifn =
    Opcode1 "lposcil" [ getExprK kamp,    getExprK kfreqrat
                      , getExprK kloop,   getExprK kend
                      , getExprUniv ifn ]


lposcil3 :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
         -> Expr ITableNum
         -> Opcode1 ARate
lposcil3 kamp kfreqrat kloop kend ifn =
    Opcode1 "lposcil3" [ getExprK kamp,     getExprK kfreqrat
                       , getExprK kloop,    getExprK kend
                       , getExprUniv ifn ]



--------------------------------------------------------------------------------
-- Models and emulations

moog :: Expr KRate -> Expr KRate -> Expr KRate 
     -> Expr KRate -> Expr KRate -> Expr KRate
     -> Expr ITableNum -> Expr ITableNum -> Expr ITableNum
     -> Opcode1 ARate
moog kamp kfreq kfiltq kfiltrate kvibf kvamp iafn iwfn ivfn =
    Opcode1 "moog" [ getExprK kamp,     getExprK kfreq
                   , getExprK kfiltq,   getExprK kfiltrate
                   , getExprK kvibf,    getExprK kvamp
                   , getExprUniv iafn,  getExprUniv iwfn
                   , getExprUniv ivfn ]


-- | idecay - default is 0
--
shaker :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr IInit
       -> Opcode1 ARate
shaker kamp kfreq kbeans kdamp ktimes idecay =
    Opcode1 "shaker" [ getExprK kamp,     getExprK kfreq
                     , getExprK kbeans,   getExprK kdamp
                     , getExprK ktimes,   getExprI idecay ]


marimba :: Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr IInit 
        -> Expr ITableNum -> Expr KRate 
        -> Expr KRate -> Expr ITableNum
        -> Expr IInit
        -> Opcode1 ARate
marimba kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec =
    Opcode1 "marimba" [ getExprK kamp,      getExprK kfreq
                      , getExprI ihrd,      getExprI ipos
                      , getExprUniv imp,    getExprK kvibf
                      , getExprK kvamp,     getExprUniv ivibfn
                      , getExprI idec ]


vibes :: Expr KRate -> Expr KRate 
      -> Expr IInit -> Expr IInit 
      -> Expr ITableNum -> Expr KRate 
      -> Expr KRate -> Expr ITableNum
      -> Expr IInit
      -> Opcode1 ARate
vibes kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec =
    Opcode1 "vibes" [ getExprK kamp,    getExprK kfreq
                    , getExprI ihrd,    getExprI ipos
                    , getExprUniv imp,  getExprK kvibf
                    , getExprK kvamp,   getExprUniv ivibfn
                    , getExprI idec ]


-- | iminfreq - default 0
--
mandol :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr ITableNum -> Expr IInit
       -> Opcode1 ARate
mandol kamp kfreq kpluck kdetune kgain ksize ifn iminfreq =
    Opcode1 "marimba" [ getExprK kamp,    getExprK kfreq
                      , getExprK kpluck,  getExprK kdetune
                      , getExprK kgain,   getExprK ksize
                      , getExprUniv ifn,  getExprI iminfreq ]


gogobel :: Expr KRate -> Expr KRate 
        -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Expr KRate -> Expr KRate -> Expr ITableNum
        -> Opcode1 ARate
gogobel kamp kfreq ihrd ipos imp kvibf kvamp ivibfn  =
    Opcode1 "gogobel" [ getExprK kamp,     getExprK kfreq
                      , getExprI ihrd,     getExprI ipos
                      , getExprI imp,      getExprK kvibf
                      , getExprK kvamp,    getExprUniv ivibfn ]


voice :: Expr KRate -> Expr KRate -> Expr KRate 
      -> Expr KRate -> Expr KRate -> Expr KRate
      -> Expr ITableNum -> Expr ITableNum 
      -> Opcode1 ARate
voice kamp kfreq kphoneme kform kvibf kvamp ifn ivfn =
    Opcode1 "voice" [ getExprK kamp,      getExprK kfreq
                    , getExprK kphoneme,  getExprK kform
                    , getExprK kvibf,     getExprK kvamp
                    , getExprUniv ifn,    getExprUniv ivfn ]


lorenz :: Expr KRate -> Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit
       -> Opcode3 ARate
lorenz ks kr kb kh ix iy iz ivx =
    Opcode3 "lorenz" [ getExprK ks, getExprK kr
                     , getExprK kb, getExprK kh
                     , getExprI ix, getExprI iy
                     , getExprI iz, getExprI ivx ]


planet :: Expr KRate -> Expr KRate -> Expr KRate 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr IInit -> Expr IInit -> Expr IInit 
       -> Expr IInit -> Expr IInit
      -> Opcode3 ARate
planet kmass1 kmass2 ksep ix iy iz ivx ivy ivz idelta ifriction =
    Opcode3 "planet" [ getExprK kmass1,   getExprK kmass2
                     , getExprK ksep,     getExprI ix
                     , getExprI iy,       getExprI iz
                     , getExprI ivx,      getExprI ivy
                     , getExprI ivz,      getExprI idelta
                     , getExprI ifriction ]

--------------------------------------------------------------------------------
-- Random noise generators


rand :: forall rate. (KA_Rate rate)
     => Expr rate -> Opcode1 rate
rand amp =
    Opcode1 "rand" [ getExprUniv amp ]


randh :: forall rate1 rate2 rate. (KA_Rate rate)
      => Expr rate1 -> Expr rate2 -> Opcode1 rate
randh amp cps =
    Opcode1 "rand" [ getExprUniv amp, getExprUniv cps ]


randi :: forall rate1 rate2 rate. (KA_Rate rate)
      => Expr rate1 -> Expr rate2 -> Opcode1 rate
randi amp cps =
    Opcode1 "rand" [ getExprUniv amp, getExprUniv cps ]


linrand :: forall rate. (Rate rate)
        => Expr KRate -> Opcode1 rate
linrand krange =
    Opcode1 "linrand" [ getExprK krange ]


trirand :: forall rate. (Rate rate)
        => Expr KRate -> Opcode1 rate
trirand krange =
    Opcode1 "trirand" [ getExprK krange ]


exprand :: forall rate. (Rate rate)
        => Expr KRate -> Opcode1 rate
exprand krange =
    Opcode1 "exprand" [ getExprK krange ]


bexprand :: forall rate. (Rate rate)
         => Expr KRate -> Opcode1 rate
bexprand krange =
    Opcode1 "bexprand" [ getExprK krange ]


cauchy :: forall rate. (Rate rate)
       => Expr KRate -> Opcode1 rate
cauchy kalpha =
    Opcode1 "cauchy" [ getExprK kalpha ]


pcauchy :: forall rate. (Rate rate)
        => Expr KRate -> Opcode1 rate
pcauchy kalpha =
    Opcode1 "pcauchy" [ getExprK kalpha ]

poisson :: forall rate. (Rate rate)
       => Expr KRate -> Opcode1 rate
poisson klambda =
    Opcode1 "poisson" [ getExprK klambda ]


gauss :: forall rate. (Rate rate)
      => Expr KRate -> Opcode1 rate
gauss krange =
    Opcode1 "gauss" [ getExprK krange ]


weibull :: forall rate. (Rate rate)
        => Expr KRate -> Expr KRate -> Opcode1 rate
weibull ksigma ktau =
    Opcode1 "weibull" [ getExprK ksigma, getExprK ktau ]


betarand :: forall rate. (Rate rate)
         => Expr KRate -> Expr KRate -> Expr KRate 
         -> Opcode1 rate
betarand krange kalpha kbeta =
    Opcode1 "betarand" [getExprK krange,  getExprK kalpha, getExprK kbeta ]


unirand :: forall rate. (Rate rate)
        => KR -> Opcode1 rate
unirand krange =
    Opcode1 "unirand" [ getExprK krange ]


