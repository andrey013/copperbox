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


import ZSnd.Core.CsoundInst
import ZSnd.Core.Inst.Click

--------------------------------------------------------------------------------
-- Linear and Exponential generators





line :: forall rate . (KA_Rate rate)
     => Conf IRate -> Conf IRate -> Conf IRate -> Element rate
line ia idur ib = 
    mkElement "line" [ getConfI ia, getConfI idur, getConfI ib ]
                     (Out1 $ dataRate (undefined :: rate))
    



expon :: forall rate. (KA_Rate rate)
      => Conf IRate -> Conf IRate -> Conf IRate 
      -> Element rate
expon ia idur ib = 
    mkElement "expon" [ getConfI ia, getConfI idur
                      , getConfI ib ]
                      (Out1 $ dataRate (undefined :: rate))

linseg :: forall rate. (KA_Rate rate)
       => Conf IRate -> Conf IRate 
       -> Conf IRate -> [(Conf IRate, Conf IRate)]
       -> Element rate
linseg ia idur ib xs = 
    mkElement "linseg" (getConfI ia : getConfI idur : getConfI ib : rest)
                       (Out1 $ dataRate (undefined :: rate))
  where
    rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs


linsegr :: forall rate. (KA_Rate rate)
        => Conf IRate -> Conf IRate 
        -> Conf IRate -> [(Conf IRate, Conf IRate)]
        -> Conf IRate -> Conf IRate 
        -> Element rate
linsegr ia idur ib xs irel iz = 
    mkElement "linsegr" (getConfI ia : getConfI idur : getConfI ib : rest ++ end)
                        (Out1 $ dataRate (undefined :: rate))
  where
    rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
    end  = [getConfI irel, getConfI iz]


expseg :: forall rate. (KA_Rate rate )
       => Conf IRate -> Conf IRate 
       -> Conf IRate -> [(Conf IRate, Conf IRate)]
       -> Element rate
expseg ia idur ib xs = 
    mkElement "expseg" (getConfI ia : getConfI idur : getConfI ib : rest)
                       (Out1 $ dataRate (undefined :: rate))
  where
    rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs


expsegr :: forall rate. (KA_Rate rate)
        => Conf IRate -> Conf IRate 
        -> Conf IRate -> [(Conf IRate, Conf IRate)]
        -> Conf IRate -> Conf IRate 
        -> Element rate
expsegr ia idur ib xs irel iz = 
    mkElement "linsegr" (getConfI ia : getConfI idur : getConfI ib : rest ++ end)
                        (Out1 $ dataRate (undefined :: rate))
  where
    rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
    end  = [getConfI irel, getConfI iz]



expsega   :: Conf IRate -> Conf IRate -> Conf IRate 
          -> [(Conf IRate, Conf IRate)]
          -> Element ARate
expsega ia idur ib xs = 
    mkElement "expsega" (getConfI ia : getConfI idur : getConfI ib : rest)
                        (Out1 A)
  where
    rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs


adsr :: forall rate. (KA_Rate rate)
     => Conf IRate -> Conf IRate -> Conf IRate -> Conf IRate 
     -> Element rate
adsr ia idec isl ir = 
    mkElement "adsr" [ getConfI ia,   getConfI idec
                     , getConfI isl,  getConfI ir ]
                     (Out1 $ dataRate (undefined :: rate))

adsr_ :: forall rate. (KA_Rate rate)
      => Conf IRate -> Conf IRate -> Conf IRate 
      -> Conf IRate -> Conf IRate 
      -> Element rate
adsr_ ia idec isl ir idel = 
    mkElement "adsr" [ getConfI ia,   getConfI idec
                     , getConfI isl,  getConfI ir
                     , getConfI idel ]
                     (Out1 $ dataRate (undefined :: rate))

madsr :: forall rate. (KA_Rate rate)
      => Conf IRate -> Conf IRate 
      -> Conf IRate -> Conf IRate 
      -> Element rate
madsr ia idec isl ir = 
    mkElement "madsr" [ getConfI ia,    getConfI idec
                      , getConfI isl,   getConfI ir ]
                      (Out1 $ dataRate (undefined :: rate))


madsr_ :: forall rate. (KA_Rate rate)
       => Conf IRate -> Conf IRate 
       -> Conf IRate -> Conf IRate 
       -> Conf IRate 
       -> Element rate
madsr_ ia idec isl ir idel = 
    mkElement "madsr" [ getConfI ia,  getConfI idec
                      , getConfI isl, getConfI ir
                      , getConfI idel ]
                      (Out1 $ dataRate (undefined :: rate))


xadsr :: forall rate. (KA_Rate rate)
      => Conf IRate -> Conf IRate 
      -> Conf IRate -> Conf IRate 
      -> Element rate
xadsr ia idec isl ir = 
    mkElement "xadsr" [ getConfI ia,    getConfI idec
                      , getConfI isl,   getConfI ir ]
                      (Out1 $ dataRate (undefined :: rate))


xadsr_ :: forall rate. (KA_Rate rate)
       => Conf IRate -> Conf IRate 
       -> Conf IRate -> Conf IRate 
       -> Conf IRate 
       -> Element rate
xadsr_ ia idec isl ir idel = 
    mkElement "xadsr" [ getConfI ia,    getConfI idec
                      , getConfI isl,   getConfI ir
                      , getConfI idel ]
                      (Out1 $ dataRate (undefined :: rate))

mxadsr :: forall rate. (KA_Rate rate)
       => Conf IRate -> Conf IRate 
       -> Conf IRate -> Conf IRate 
       -> Element rate
mxadsr ia idec isl ir = 
    mkElement "mxadsr" [ getConfI ia,   getConfI idec
                       , getConfI isl,  getConfI ir ]
                       (Out1 $ dataRate (undefined :: rate))

mxadsr_ :: forall rate. (KA_Rate rate)
        => Conf IRate -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate 
        -> Element rate
mxadsr_ ia idec isl ir idel = 
    mkElement "mxadsr" [ getConfI ia,   getConfI idec
                       , getConfI isl,  getConfI ir
                       , getConfI idel ]
                       (Out1 $ dataRate (undefined :: rate))


--------------------------------------------------------------------------------
-- Table access

table :: forall rate1 rate. (Rate rate)
      => Conf rate1 -> Conf IRate 
      -> Element rate
table ndx ifn = 
    mkElement "table" [ getConfUniv ndx, getConfI ifn ]
                      (Out1 $ dataRate (undefined :: rate))

table_ :: forall rate1 rate. (Rate rate)
       => Conf rate1 -> Conf IRate -> Conf IRate -> Conf IRate -> Conf IRate    
       -> Element rate
table_ ndx ifn ixmode ixoff ixwrap = 
    mkElement "table" [ getConfUniv ndx,    getConfI ifn
                      , getConfI ixmode,    getConfI ixoff
                      , getConfI ixwrap ]
                      (Out1 $ dataRate (undefined :: rate))


tablei :: forall rate1 rate. (Rate rate)
       => Conf rate1 -> Conf IRate 
       -> Element rate
tablei ndx ifn = 
    mkElement "tablei" [ getConfUniv ndx, getConfI ifn ]
                       (Out1 $ dataRate (undefined :: rate))

tablei_ :: forall rate1 rate. (Rate rate)
        => Conf rate1 -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate
       -> Element rate
tablei_ ndx ifn ixmode ixoff ixwrap = 
    mkElement "tablei" [ getConfUniv ndx,   getConfI ifn
                       , getConfI ixmode,   getConfI ixoff
                       , getConfI ixwrap ]
                       (Out1 $ dataRate (undefined :: rate))



table3 :: forall rate1 rate. (Rate rate)
       => Conf rate1 -> Conf IRate 
       -> Element rate
table3 ndx ifn = 
    mkElement "table" [ getConfUniv ndx, getConfI ifn ]
                       (Out1 $ dataRate (undefined :: rate))

table3_ :: forall rate1 rate. (Rate rate)
        => Conf rate1 -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate
        -> Element rate
table3_ ndx ifn ixmode ixoff ixwrap = 
    mkElement "table3" [ getConfUniv ndx,   getConfI ifn
                       , getConfI ixmode,   getConfI ixoff
                       , getConfI ixwrap ]
                       (Out1 $ dataRate (undefined :: rate))


oscil1 :: Conf IRate -> Conf KRate 
       -> Conf IRate -> Conf IRate 
       -> Element KRate
oscil1 idel kamp idur ifn = 
    mkElement "oscil1" [ getConfI idel,   getConfK kamp
                       , getConfI idur,   getConfI ifn ]
                       (Out1 K)

oscil1i :: Conf IRate -> Conf KRate 
        -> Conf IRate -> Conf IRate 
        -> Element KRate
oscil1i idel kamp idur ifn = 
    mkElement "oscil1i" [ getConfI idel,  getConfK kamp
                        , getConfI idur,  getConfI ifn ]
                        (Out1 K)

osciln :: Conf KRate -> Conf IRate 
       -> Conf IRate -> Conf IRate 
       -> Element KRate
osciln kamp ifrq ifn itimes = 
    mkElement "osciln" [ getConfK kamp,   getConfI ifrq
                       , getConfI ifn,    getConfI itimes ]
                       (Out1 K)
--------------------------------------------------------------------------------
-- Phasors

-- class CPhasor rate where

 
phasor :: forall rate1 rate. (KA_Rate rate)
       => Conf rate1 
       -> Element rate
phasor cps = 
    mkElement "phasor" [getConfUniv cps]
                       (Out1 $ dataRate (undefined :: rate))


phasor_ :: forall rate1 rate. (KA_Rate rate)
        => Conf rate1 -> Conf IRate 
        -> Element rate
phasor_ cps iphs = 
    mkElement "phasor" [getConfUniv cps, getConfI iphs]
                       (Out1 $ dataRate (undefined :: rate))
   

phasorbnk :: forall rate1 rate. (KA_Rate rate)
          => Conf rate1 -> Conf KRate -> Conf IRate 
          -> Element rate
phasorbnk cps kindx icnt = 
    mkElement "phasorbnk" [ getConfUniv cps,  getConfK kindx
                          , getConfI icnt ]
                          (Out1 $ dataRate (undefined :: rate))


phasorbnk_  :: forall rate1 rate. (KA_Rate rate)
            => Conf rate1 -> Conf KRate 
            -> Conf IRate -> Conf IRate 
            -> Element rate
phasorbnk_ cps kindx icnt iphs = 
    mkElement "phasorbnk" [ getConfUniv cps,  getConfK kindx
                          , getConfI icnt, getConfI iphs ]
                          (Out1 $ dataRate (undefined :: rate))


--------------------------------------------------------------------------------
-- Basic oscillators

-- | Note for A rate, cps can seemingly be any type.
-- 
oscil :: forall r1 r2 rate. (KA_Rate rate)
      => Conf r1 -> Conf r2 -> Conf IRate 
      -> Element rate
oscil amp cps ifn  = 
    mkElement "oscil" [ getConfUniv amp,  getConfUniv cps
                      , getConfI ifn ]
                      (Out1 $ dataRate (undefined :: rate))

oscil_ :: forall r1 r2 rate. (KA_Rate rate)
       => Conf r1  -> Conf r2
       -> Conf IRate -> Conf IRate 
       -> Element rate
oscil_ amp cps ifn iphs = 
    mkElement "oscil" [ getConfUniv amp,  getConfUniv cps
                      , getConfI ifn,     getConfI iphs ]
                      (Out1 $ dataRate (undefined :: rate))

oscili :: forall r1 r2 rate. (KA_Rate rate)
       => Conf r1 -> Conf r2 -> Conf IRate 
       -> Element rate
oscili amp cps ifn = 
    mkElement "oscili" [ getConfUniv amp,   getConfUniv cps
                       , getConfI ifn]
                       (Out1 $ dataRate (undefined :: rate))

oscili_ :: forall r1 r2 rate. (KA_Rate rate)
        => Conf r1 -> Conf r2 
        -> Conf IRate -> Conf IRate 
        -> Element rate
oscili_ amp cps ifn iphs = 
    mkElement "oscili" [ getConfUniv amp,   getConfUniv cps
                       , getConfI ifn,      getConfI iphs ]
                       (Out1 $ dataRate (undefined :: rate))

oscil3 :: forall r1 r2 rate. (KA_Rate rate)
       => Conf r1 -> Conf r2 -> Conf IRate 
       -> Element rate
oscil3 amp cps ifn = 
    mkElement "oscil3" [ getConfUniv amp,   getConfUniv cps
                       , getConfI ifn ]
                       (Out1 $ dataRate (undefined :: rate))

oscil3_ :: forall r1 r2 rate. (KA_Rate rate)
        => Conf r1 -> Conf r2
        -> Conf IRate -> Conf IRate 
        -> Element rate
oscil3_ amp cps ifn iphs = 
    mkElement "oscil3" [ getConfUniv amp,   getConfUniv cps
                       , getConfI ifn,      getConfI iphs ]
                       (Out1 $ dataRate (undefined :: rate))

poscil :: forall r1 r2 rate. (KA_Rate rate)
       => Conf r1 -> Conf r2 -> Conf IRate 
       -> Element rate
poscil amp cps ift = 
    mkElement "poscil" [ getConfUniv amp,   getConfUniv cps
                       , getConfI ift ]
                       (Out1 $ dataRate (undefined :: rate))

poscil_ :: forall r1 r2 rate. (KA_Rate rate)
        => Conf r1 -> Conf r2 -> Conf IRate -> Conf IRate 
        -> Element rate
poscil_ amp cps ift iphs = 
    mkElement "poscil" [ getConfUniv amp,   getConfUniv cps
                       , getConfI ift,      getConfI iphs ]
                       (Out1 $ dataRate (undefined :: rate))


poscil3 :: forall r1 r2 rate. (KA_Rate rate)
        => Conf r1 -> Conf r2 -> Conf IRate 
        -> Element rate
poscil3 amp cps ift = 
    mkElement "poscil3" [ getConfUniv amp,  getConfUniv cps
                        , getConfI ift ]
                        (Out1 $ dataRate (undefined :: rate))


poscil3_  :: forall r1 r2 rate. (KA_Rate rate)
          => Conf r1 -> Conf r2 
          -> Conf IRate -> Conf IRate 
          -> Element rate
poscil3_ amp cps ift iphs = 
    mkElement "poscil3" [ getConfUniv amp,  getConfUniv cps
                        , getConfI ift,     getConfI iphs ]
                        (Out1 $ dataRate (undefined :: rate))


lfo :: forall r1 r2 rate. (KA_Rate rate)
    => Conf r1 -> Conf r2
    -> Element rate
lfo amp cps = 
    mkElement "lfo" [ getConfUniv amp,  getConfUniv cps]
                    (Out1 $ dataRate (undefined :: rate))


lfo_ :: forall r1 r2 rate. (KA_Rate rate)
     => Conf r1 -> Conf r2 -> Conf IRate 
     -> Element rate
lfo_ amp cps itype = 
    mkElement "lfo" [ getConfUniv amp,  getConfUniv cps
                    , getConfI itype ]
                    (Out1 $ dataRate (undefined :: rate))



--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this 
-- correct?

buzz :: Conf r1 -> Conf r2
     -> Conf KRate -> Conf IRate 
     -> Element ARate
buzz xamp xcps knh ifn = 
    mkElement "buzz" [ getConfUniv xamp,  getConfUniv xcps
                     , getConfK knh,      getConfI ifn ]
                     (Out1 A)

buzz_ :: Conf r1 -> Conf r2 -> Conf KRate 
      -> Conf IRate -> Conf IRate 
      -> Element ARate
buzz_ xamp xcps knh ifn iphs = 
    mkElement "buzz" [ getConfUniv xamp,  getConfUniv xcps
                     , getConfK knh,      getConfI ifn
                     , getConfI iphs ]
                     (Out1 A)


gbuzz :: Conf r1 -> Conf r2
      -> Conf KRate -> Conf KRate 
      -> Conf KRate -> Conf IRate 
      -> Element ARate
gbuzz xamp xcps knh klh kr ifn = 
    mkElement "gbuzz" [ getConfUniv xamp,   getConfUniv xcps
                      , getConfK knh,       getConfK klh
                      , getConfK kr,        getConfI ifn ]
                      (Out1 A)

gbuzz_ :: Conf r1 -> Conf r2
       -> Conf KRate -> Conf KRate -> Conf KRate 
       -> Conf IRate -> Conf IRate
       -> Element ARate
gbuzz_ xamp xcps knh klh kr ifn iphs = 
    mkElement "gbuzz" [ getConfUniv xamp,   getConfUniv xcps
                      , getConfK knh,       getConfK klh
                      , getConfK kr,        getConfI ifn
                      , getConfI iphs ]
                      (Out1 A)

vco :: Conf KRate -> Conf KRate 
    -> Conf IRate -> Conf KRate 
    -> Conf IRate -> Conf IRate 
    -> Element ARate
vco kamp kfqc iwave kpw ifn imaxd = 
    mkElement "gbuzz" [ getConfK kamp,    getConfK kfqc
                      , getConfI iwave,   getConfK kpw
                      , getConfI ifn,     getConfI imaxd ]
                      (Out1 A)

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Conf KRate -> Conf KRate 
      -> Conf KRate -> Conf IRate 
      -> Element ARate
adsyn kamod kfmod ksmod ifilcod = 
    mkElement "adsyn" [ getConfK kamod,   getConfK kfmod
                      , getConfK ksmod,   getConfI ifilcod ]
                      (Out1 A)


adsynt :: Conf KRate -> Conf KRate 
       -> Conf IRate -> Conf IRate 
       -> Conf IRate -> Conf IRate
       -> Element ARate
adsynt kamp kcps iwfn ifreqfn iampfn icnt = 
    mkElement "adsynt" [ getConfK kamp,     getConfK kcps
                       , getConfI iwfn,     getConfI ifreqfn
                       , getConfI iampfn,   getConfI icnt ]
                       (Out1 A)

adsynt_ :: Conf KRate -> Conf KRate 
        -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate
        -> Conf IRate
        -> Element ARate
adsynt_ kamp kcps iwfn ifreqfn iampfn icnt iphs = 
    mkElement "adsynt" [ getConfK kamp,     getConfK kcps
                       , getConfI iwfn,     getConfI ifreqfn
                       , getConfI iampfn,   getConfI icnt
                       , getConfI iphs ]
                       (Out1 A)


hsboscil :: Conf KRate -> Conf KRate 
         -> Conf KRate -> Conf IRate 
         -> Conf IRate -> Conf IRate
         -> Element ARate
hsboscil kamp ktone kbrite ibasfreq iwfn ioctfn = 
    mkElement "hsboscil" [ getConfK kamp,     getConfK ktone
                         , getConfK kbrite,   getConfI ibasfreq
                         , getConfI iwfn,     getConfI ioctfn ]
                         (Out1 A)

hsboscil_ :: Conf KRate -> Conf KRate 
          -> Conf KRate -> Conf IRate 
          -> Conf IRate -> Conf IRate
          -> Conf IRate
          -> Element ARate
hsboscil_ kamp ktone kbrite ibasfreq iwfn ioctfn iphs = 
    mkElement "hsboscil" [ getConfK kamp,     getConfK ktone
                         , getConfK kbrite,   getConfI ibasfreq
                         , getConfI iwfn,     getConfI ioctfn
                         , getConfI iphs ]
                         (Out1 A)

--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Conf rate -> Conf KRate 
       -> Conf KRate -> Conf KRate 
       -> Conf KRate -> Conf IRate
       -> Element ARate
foscil xamp kcps kcar kmod kndx ifn = 
    mkElement "foscil" [ getConfUniv xamp,  getConfK kcps
                       , getConfK kcar,     getConfK kmod
                       , getConfK kndx,     getConfI ifn ]
                       (Out1 A)

foscil_ :: Conf rate -> Conf KRate 
        -> Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf IRate 
        -> Conf IRate
        -> Element ARate
foscil_ xamp kcps kcar kmod kndx ifn iphs = 
    mkElement "foscil" [ getConfUniv xamp,  getConfK kcps
                       , getConfK kcar,     getConfK kmod
                       , getConfK kndx,     getConfI ifn
                       , getConfI iphs ]
                       (Out1 A)

foscili :: Conf rate -> Conf KRate 
        -> Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf IRate
        -> Element ARate
foscili xamp kcps kcar kmod kndx ifn = 
    mkElement "foscili" [ getConfUniv xamp,   getConfK kcps
                        , getConfK kcar,      getConfK kmod
                        , getConfK kndx,      getConfI ifn ]
                        (Out1 A)


foscili_ :: Conf rate -> Conf KRate 
         -> Conf KRate -> Conf KRate 
         -> Conf KRate -> Conf IRate 
         -> Conf IRate
         -> Element ARate
foscili_ xamp kcps kcar kmod kndx ifn iphs = 
    mkElement "foscil" [ getConfUniv xamp,  getConfK kcps
                       , getConfK kcar,     getConfK kmod
                       , getConfK kndx,     getConfI ifn
                       , getConfI iphs ]
                       (Out1 A)


fmvoice :: Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate
        -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate 
        -> Conf IRate
        -> Element ARate
fmvoice kamp kfreq kvowel ktilt kvibamt kvibrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmvoice" [ getConfK kamp,      getConfK kfreq
                        , getConfK kvowel,    getConfK ktilt
                        , getConfK kvibamt,   getConfK kvibrate
                        , getConfI ifn1,      getConfI ifn2
                        , getConfI ifn3,      getConfI ifn4
                        , getConfI ivibfn ]
                        (Out1 A)

fmbell :: Conf KRate -> Conf KRate 
       -> Conf KRate -> Conf KRate 
       -> Conf KRate -> Conf KRate
       -> Conf IRate -> Conf IRate 
       -> Conf IRate -> Conf IRate 
       -> Conf IRate
       -> Element ARate
fmbell kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmbell" [ getConfK kamp,     getConfK kfreq
                       , getConfK kc1,      getConfK kc2
                       , getConfK kvdepth,  getConfK kvrate
                       , getConfI ifn1,     getConfI ifn2
                       , getConfI ifn3,     getConfI ifn4
                       , getConfI ivibfn ]
                       (Out1 A)

fmrhode :: Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate
        -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate 
        -> Conf IRate
        -> Element ARate
fmrhode kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmrhode" [ getConfK kamp,      getConfK kfreq
                        , getConfK kc1,       getConfK kc2
                        , getConfK kvdepth,   getConfK kvrate
                        , getConfI ifn1,      getConfI ifn2
                        , getConfI ifn3,      getConfI ifn4
                        , getConfI ivibfn ]
                        (Out1 A)


fmwurlie :: Conf KRate -> Conf KRate 
         -> Conf KRate -> Conf KRate 
         -> Conf KRate -> Conf KRate
         -> Conf IRate -> Conf IRate 
         -> Conf IRate -> Conf IRate 
         -> Conf IRate
         -> Element ARate
fmwurlie kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmwurlie" [ getConfK kamp,     getConfK kfreq
                         , getConfK kc1,      getConfK kc2
                         , getConfK kvdepth,  getConfK kvrate
                         , getConfI ifn1,     getConfI ifn2
                         , getConfI ifn3,     getConfI ifn4
                         , getConfI ivibfn ]
                         (Out1 A)

fmmetal :: Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate
        -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate 
        -> Conf IRate
        -> Element ARate
fmmetal kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmmetal" [ getConfK kamp,      getConfK kfreq
                        , getConfK kc1,       getConfK kc2
                        , getConfK kvdepth,   getConfK kvrate
                        , getConfI ifn1,      getConfI ifn2
                        , getConfI ifn3,      getConfI ifn4
                        , getConfI ivibfn ]
                        (Out1 A)

fmb3 :: Conf KRate -> Conf KRate 
     -> Conf KRate -> Conf KRate 
     -> Conf KRate -> Conf KRate
     -> Conf IRate -> Conf IRate 
     -> Conf IRate -> Conf IRate 
     -> Conf IRate
     -> Element ARate
fmb3 kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmb3" [ getConfK kamp,     getConfK kfreq
                     , getConfK kc1,      getConfK kc2
                     , getConfK kvdepth,  getConfK kvrate
                     , getConfI ifn1,     getConfI ifn2
                     , getConfI ifn3,     getConfI ifn4
                     , getConfI ivibfn ]
                     (Out1 A)


fmpercfl :: Conf KRate -> Conf KRate
         -> Conf KRate -> Conf KRate 
         -> Conf KRate -> Conf KRate
         -> Conf IRate -> Conf IRate 
         -> Conf IRate -> Conf IRate 
         -> Conf IRate
         -> Element ARate
fmpercfl kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    mkElement "fmpercfl" [ getConfK kamp,     getConfK kfreq
                         , getConfK kc1,      getConfK kc2
                         , getConfK kvdepth,  getConfK kvrate
                         , getConfI ifn1,     getConfI ifn2
                         , getConfI ifn3,     getConfI ifn4
                         , getConfI ivibfn ]
                         (Out1 A)

--------------------------------------------------------------------------------
-- Sample playback

-- Note - it seems idiomatic to want a stereo version will only 
-- the mandatory args...


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil :: Conf a -> Conf KRate -> Conf IRate -> Conf IRate -> Conf IRate
       -> Element ARate
loscil xamp kcps ifn ibase imod1 = 
    mkElement "loscil" [ getConfUniv xamp,  getConfK kcps
                       , getConfI ifn,      getConfI ibase
                       , getConfI imod1 ]
                       (Out1 A)


-- | Stereo version of 'loscil'.
-- 
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil :: Conf a -> Conf KRate -> Conf IRate -> Conf IRate -> Conf IRate
       -> Element ARate
biloscil xamp kcps ifn ibase imod1 = 
    mkElement "loscil" [ getConfUniv xamp,  getConfK kcps
                       , getConfI ifn,      getConfI ibase
                       , getConfI imod1 ]
                       (Out2 A)

-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil3 :: Conf a -> Conf KRate -> Conf IRate -> Conf IRate -> Conf IRate
       -> Element ARate
loscil3 xamp kcps ifn ibase imod1 = 
    mkElement "loscil3" [ getConfUniv xamp,   getConfK kcps
                        , getConfI ifn,       getConfI ibase
                        , getConfI imod1 ]
                        (Out1 A)


-- | Stereo version of 'loscil3'.
-- 
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil3 :: Conf rate -> Conf KRate 
          -> Conf IRate -> Conf IRate 
          -> Conf IRate
          -> Element ARate
biloscil3 xamp kcps ifn ibase imod1 = 
    mkElement "loscil3" [ getConfUniv xamp,  getConfK kcps
                        , getConfI ifn,      getConfI ibase
                        , getConfI  imod1 ]
                        (Out2 A)

lposcil :: Conf KRate -> Conf KRate 
        -> Conf KRate -> Conf KRate 
        -> Conf IRate
        -> Element ARate
lposcil kamp kfreqrat kloop kend ift = 
    mkElement "lposcil" [ getConfK kamp,    getConfK kfreqrat
                        , getConfK kloop,   getConfK kend
                        , getConfI ift ]
                        (Out1 A)

lposcil3 :: Conf KRate -> Conf KRate -> Conf KRate 
         -> Conf KRate -> Conf IRate
         -> Element ARate
lposcil3 kamp kfreqrat kloop kend ift = 
    mkElement "lposcil3" [ getConfK kamp,   getConfK kfreqrat
                         , getConfK kloop,  getConfK kend
                         , getConfI ift ]
                         (Out1 A)



--------------------------------------------------------------------------------
-- Models and emulations

moog :: Conf KRate -> Conf KRate -> Conf KRate 
     -> Conf KRate -> Conf KRate -> Conf KRate
     -> Conf IRate -> Conf IRate -> Conf IRate
     -> Element ARate
moog kamp kfreq kfiltq kfiltrate kvibf kvamp iafn iwfn ivfn = 
    mkElement "moog" [ getConfK kamp,     getConfK kfreq
                     , getConfK kfiltq,   getConfK kfiltrate
                     , getConfK kvibf,    getConfK kvamp 
                     , getConfI iafn,     getConfI iwfn
                     , getConfI ivfn ]
                     (Out1 A)
--------------------------------------------------------------------------------
-- Random noise generators


rand :: forall rate. (KA_Rate rate)
     => Conf rate -> Element rate
rand amp = 
    mkElement "rand" [getConfUniv amp]
                     (Out1 $ dataRate (undefined :: rate))

randh :: forall rate. (KA_Rate rate)
      => Conf rate -> Conf rate -> Element rate
randh amp cps = 
    mkElement "rand" [getConfUniv amp, getConfUniv cps]
                     (Out1 $ dataRate (undefined :: rate))

randi :: forall rate. (KA_Rate rate)
      => Conf rate -> Conf rate -> Element rate
randi amp cps = 
    mkElement "rand" [getConfUniv amp, getConfUniv cps]
                     (Out1 $ dataRate (undefined :: rate))

linrand :: forall rate. (Rate rate)
        => Conf KRate -> Element rate
linrand krange = 
    mkElement "linrand" [getConfK krange]
                        (Out1 $ dataRate (undefined :: rate))

trirand :: forall rate. (Rate rate)
        => Conf KRate -> Element rate
trirand krange = 
    mkElement "trirand" [getConfK krange]
                        (Out1 $ dataRate (undefined :: rate))

exprand :: forall rate. (Rate rate)
        => Conf KRate -> Element rate
exprand krange = 
    mkElement "exprand" [getConfK krange]
                        (Out1 $ dataRate (undefined :: rate))

bexprand :: forall rate. (Rate rate)
         => Conf KRate -> Element rate
bexprand krange = 
    mkElement "bexprand" [getConfK krange]
                        (Out1 $ dataRate (undefined :: rate))

cauchy :: forall rate. (Rate rate)
       => Conf KRate -> Element rate
cauchy kalpha = 
    mkElement "cauchy" [getConfK kalpha]
                        (Out1 $ dataRate (undefined :: rate))


pcauchy :: forall rate. (Rate rate)
        => Conf KRate -> Element rate
pcauchy kalpha = 
    mkElement "pcauchy" [getConfK kalpha]
                        (Out1 $ dataRate (undefined :: rate))

poisson :: forall rate. (Rate rate)
       => Conf KRate -> Element rate
poisson klambda = 
    mkElement "poisson" [getConfK klambda]
                        (Out1 $ dataRate (undefined :: rate))

gauss :: forall rate. (Rate rate)
      => Conf KRate -> Element rate
gauss krange = 
    mkElement "gauss" [getConfK krange]
                      (Out1 $ dataRate (undefined :: rate))

weibull :: forall rate. (Rate rate)
        => Conf KRate -> Conf KRate -> Element rate
weibull ksigma ktau = 
    mkElement "weibull" [getConfK ksigma, getConfK ktau]
                        (Out1 $ dataRate (undefined :: rate))

betarand :: forall rate. (Rate rate)
         => Conf KRate -> Conf KRate -> Conf KRate -> Element rate
betarand krange kalpha kbeta = 
    mkElement "betarand" [getConfK krange, getConfK kalpha, getConfK kbeta]
                         (Out1 $ dataRate (undefined :: rate))

unirand :: forall rate. (Rate rate)
        => Conf KRate -> Element rate
unirand krange = 
    mkElement "unirand" [getConfK krange]
                        (Out1 $ dataRate (undefined :: rate))

