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
  , LinsegConfig
  , LinsegOpcode
  , linseg

  , LinsegrConfig
  , LinsegrOpcode
  , linsegr

  , ExpsegConfig
  , ExpsegOpcode
  , expseg

  , ExpsegrConfig
  , ExpsegrOpcode
  , expsegr

  , ExpsegaConfig
  , ExpsegaOpcode
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


import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Index
import ZSnd.Core.CsoundInst.Typed

--------------------------------------------------------------------------------
-- Linear and Exponential generators





line :: forall rate . (KA_Rate rate)
     => Opcode3 IInit IInit IInit -> Element rate
line opF = 
    mkOpcode "line" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib) -> 
                [ getConfI ia, getConfI idur, getConfI ib ] 
    



expon :: forall rate. (KA_Rate rate)
      => Opcode3 IInit IInit IInit -> Element rate
expon opF = 
    mkOpcode "expon" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idur,ib) -> 
                [ getConfI ia, getConfI idur, getConfI ib ]


-- Complex input signatures are a pain...

type LinsegConfig = ( Conf IInit, Conf IInit
                    , Conf IInit, [(Conf IInit, Conf IInit)] )
                      
type LinsegOpcode = ElemRef -> PortDict -> Either FailMsg LinsegConfig


linseg :: forall rate. (KA_Rate rate)
       => LinsegOpcode -> Element rate
linseg opF = 
    mkOpcode "linseg" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                in (getConfI ia : getConfI idur : getConfI ib : rest)
    

type LinsegrConfig = ( Conf IInit, Conf IInit
                     , Conf IInit, [(Conf IInit, Conf IInit)]
                     , Conf IInit, Conf IInit )
                      
type LinsegrOpcode = ElemRef -> PortDict -> Either FailMsg LinsegrConfig


linsegr :: forall rate. (KA_Rate rate)
        => LinsegrOpcode -> Element rate
linsegr opF = 
    mkOpcode "linsegr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs, irel, iz) ->
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                    end  = [getConfI irel, getConfI iz]
                in (getConfI ia : getConfI idur : getConfI ib : rest ++ end)


type ExpsegConfig = ( Conf IInit, Conf IInit
                    , Conf IInit, [(Conf IInit, Conf IInit)])
                      
type ExpsegOpcode = ElemRef -> PortDict -> Either FailMsg ExpsegConfig


expseg :: forall rate. (KA_Rate rate )
       => ExpsegOpcode -> Element rate
expseg opF = 
    mkOpcode "expseg" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                in (getConfI ia : getConfI idur : getConfI ib : rest)

type ExpsegrConfig = ( Conf IInit, Conf IInit
                     , Conf IInit, [(Conf IInit, Conf IInit)]
                     , Conf IInit, Conf IInit )
                      
type ExpsegrOpcode = ElemRef -> PortDict -> Either FailMsg ExpsegrConfig

expsegr :: forall rate. (KA_Rate rate)
        => ExpsegrOpcode -> Element rate
expsegr opF = 
    mkOpcode "linsegr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs, irel, iz) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                    end  = [getConfI irel, getConfI iz]
                in (getConfI ia : getConfI idur : getConfI ib : rest ++ end)


type ExpsegaConfig = ( Conf IInit, Conf IInit
                     , Conf IInit, [(Conf IInit, Conf IInit)])
                      
type ExpsegaOpcode = ElemRef -> PortDict -> Either FailMsg ExpsegaConfig


expsega :: ExpsegaOpcode -> Element ARate
expsega opF = 
    mkOpcode "expsega" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(ia,idur,ib,xs) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                in (getConfI ia : getConfI idur : getConfI ib : rest)

adsr :: forall rate. (KA_Rate rate)
     => Opcode4 IInit IInit IInit IInit -> Element rate
adsr opF =
    mkOpcode "adsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,   getConfI idec
                , getConfI isl,  getConfI ir ]

adsr_ :: forall rate. (KA_Rate rate)
      => Opcode5 IInit IInit IInit IInit IInit -> Element rate
adsr_ opF =
    mkOpcode "adsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) ->
               [ getConfI ia,   getConfI idec
               , getConfI isl,  getConfI ir
               , getConfI idel ]


madsr :: forall rate. (KA_Rate rate)
      => Opcode4 IInit IInit IInit IInit -> Element rate
madsr opF = 
    mkOpcode "madsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir ]

madsr_ :: forall rate. (KA_Rate rate)
       => Opcode5 IInit IInit IInit IInit IInit -> Element rate
madsr_ opF =
    mkOpcode "madsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) -> 
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir
                , getConfI idel ]

xadsr :: forall rate. (KA_Rate rate)
      => Opcode4 IInit IInit IInit IInit -> Element rate
xadsr opF =
    mkOpcode "xadsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir ]


xadsr_ :: forall rate. (KA_Rate rate)
       => Opcode5 IInit IInit IInit IInit IInit -> Element rate
xadsr_ opF =
    mkOpcode "xadsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) ->
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir
                , getConfI idel ]

mxadsr :: forall rate. (KA_Rate rate)
       => Opcode4 IInit IInit IInit IInit -> Element rate
mxadsr opF =
    mkOpcode "mxadsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,   getConfI idec
                , getConfI isl,  getConfI ir ]

mxadsr_ :: forall rate. (KA_Rate rate)
        => Opcode5 IInit IInit IInit IInit IInit -> Element rate
mxadsr_ opF =
    mkOpcode "mxadsr" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) ->
                [ getConfI ia,   getConfI idec
                , getConfI isl,  getConfI ir
                , getConfI idel ]


--------------------------------------------------------------------------------
-- Table access

table :: forall rate1 rate. (Rate rate)
      => Int -> Opcode1 rate1 -> Element rate
table ifn opF =
    mkOpcode "table" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \ndx ->
                [ getConfUniv ndx, getConfI $ tablefn ifn ]

table_ :: forall rate1 rate. (Rate rate)
       => Int -> Opcode4 rate1 IInit IInit IInit -> Element rate
table_ ifn opF =
    mkOpcode "table" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ixmode,ixoff,ixwrap) ->
                [ getConfUniv ndx,    getConfI $ tablefn ifn
                , getConfI ixmode,    getConfI ixoff
                , getConfI ixwrap ]

tablei :: forall rate1 rate. (Rate rate)
       => Int -> Opcode1 rate1 -> Element rate
tablei ifn opF =
    mkOpcode "tablei" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \ndx ->
                [ getConfUniv ndx, getConfI $ tablefn ifn ]

tablei_ :: forall rate1 rate. (Rate rate)
        => Int -> Opcode4 rate1 IInit IInit IInit -> Element rate
tablei_ ifn opF =
    mkOpcode "tablei" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ixmode,ixoff,ixwrap) ->
                [ getConfUniv ndx,   getConfI $ tablefn ifn
                , getConfI ixmode,   getConfI ixoff
                , getConfI ixwrap ]



table3 :: forall rate1 rate. (Rate rate)
       => Int -> Opcode1 rate1 -> Element rate
table3 ifn opF =
    mkOpcode "table" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \ndx ->
             [ getConfUniv ndx, getConfI $ tablefn ifn ]


table3_ :: forall rate1 rate. (Rate rate)
        => Int -> Opcode4 rate1 IInit IInit IInit -> Element rate
table3_ ifn opF =
    mkOpcode "table3" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ixmode,ixoff,ixwrap) ->
                [ getConfUniv ndx,   getConfI $ tablefn ifn
                , getConfI ixmode,   getConfI ixoff
                , getConfI ixwrap ]


oscil1 :: Int -> Opcode3 IInit KRate IInit -> Element KRate
oscil1 ifn opF =
    mkOpcode "oscil1" inspec [ifn] (Out1 K)
  where
    inspec = applyOpcode opF $ \(idel,kamp,idur) ->
               [ getConfI idel,   getConfK kamp
               , getConfI idur,   getConfI $ tablefn ifn ]

oscil1i :: Int -> Opcode3 IInit KRate IInit -> Element KRate
oscil1i ifn opF =
    mkOpcode "oscil1i" inspec [ifn] (Out1 K)
  where
    inspec = applyOpcode opF $ \(idel,kamp,idur) ->
                [ getConfI idel,  getConfK kamp
                , getConfI idur,  getConfI $ tablefn ifn ]

osciln :: Int -> Opcode3 KRate IInit IInit -> Element KRate
osciln ifn opF =
    mkOpcode "osciln" inspec [ifn] (Out1 K)
  where
    inspec = applyOpcode opF $ \(kamp,ifrq,itimes) -> 
                [ getConfK kamp,            getConfI ifrq
                , getConfI $ tablefn ifn,   getConfI itimes ]

--------------------------------------------------------------------------------
-- Phasors

-- class CPhasor rate where


phasor :: forall rate1 rate. (KA_Rate rate)
       => Opcode1 rate1 -> Element rate
phasor opF =
    mkOpcode "phasor" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \cps ->
                [ getConfUniv cps ]


phasor_ :: forall rate1 rate. (KA_Rate rate)
        => Opcode2 rate1 IInit -> Element rate
phasor_ opF =
    mkOpcode "phasor" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(cps,iphs) ->
                [ getConfUniv cps, getConfI iphs ]


phasorbnk :: forall rate1 rate. (KA_Rate rate)
          => Opcode3 rate1 KRate IInit -> Element rate
phasorbnk opF =
    mkOpcode "phasorbnk" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(cps,kindx,icnt) ->
                [ getConfUniv cps,  getConfK kindx
                , getConfI icnt ]


phasorbnk_  :: forall rate1 rate. (KA_Rate rate)
            => Opcode4 rate1 KRate IInit IInit -> Element rate
phasorbnk_ opF = 
    mkOpcode "phasorbnk" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(cps,kindx,icnt,iphs) ->
                [ getConfUniv cps,  getConfK kindx
                , getConfI icnt,    getConfI iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

-- | Note for A rate, cps can seemingly be any type.
-- 
oscil :: forall r1 r2 rate. (KA_Rate rate)
      => Int -> Opcode2 r1 r2 -> Element rate
oscil ifn opF = 
    mkOpcode "oscil" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn ]


oscil_ :: forall r1 r2 rate. (KA_Rate rate)
       => Int -> Opcode3 r1 r2  IInit -> Element rate
oscil_ ifn opF =
    mkOpcode "oscil" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,iphs) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn,   getConfI iphs ]

oscili :: forall r1 r2 rate. (KA_Rate rate)
       => Int -> Opcode2 r1 r2 -> Element rate
oscili ifn opF = 
    mkOpcode "oscili" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn ]

oscili_ :: forall r1 r2 rate. (KA_Rate rate)
        => Int -> Opcode3 r1 r2 IInit -> Element rate
oscili_ ifn opF =
    mkOpcode "oscili" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,iphs) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn,   getConfI iphs ]

oscil3 :: forall r1 r2 rate. (KA_Rate rate)
       => Int -> Opcode2 r1 r2 -> Element rate
oscil3 ifn opF =
    mkOpcode "oscil3" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn ]

oscil3_ :: forall r1 r2 rate. (KA_Rate rate)
        => Int -> Opcode3 r1 r2 IInit -> Element rate
oscil3_ ifn opF = 
    mkOpcode "oscil3" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,iphs) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn,   getConfI iphs ]

poscil :: forall r1 r2 rate. (KA_Rate rate)
       => Int -> Opcode2 r1 r2 -> Element rate
poscil ifn opF = 
    mkOpcode "poscil" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn ]

poscil_ :: forall r1 r2 rate. (KA_Rate rate)
        => Int -> Opcode3 r1 r2 IInit -> Element rate
poscil_ ifn opF = 
    mkOpcode "poscil" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,iphs) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn,   getConfI iphs ]


poscil3 :: forall r1 r2 rate. (KA_Rate rate)
        => Int -> Opcode2 r1 r2 -> Element rate
poscil3 ifn opF = 
    mkOpcode "poscil3" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn ]


poscil3_  :: forall r1 r2 rate. (KA_Rate rate)
          => Int -> Opcode3 r1 r2 IInit -> Element rate
poscil3_ ifn opF = 
    mkOpcode "poscil3" inspec [ifn] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,iphs) ->
                [ getConfUniv amp,          getConfUniv cps
                , getConfI $ tablefn ifn,   getConfI iphs ]


lfo :: forall r1 r2 rate. (KA_Rate rate)
    => Opcode2 r1 r2 -> Element rate
lfo opF = 
    mkOpcode "lfo" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,  getConfUniv cps]


lfo_ :: forall r1 r2 rate. (KA_Rate rate)
     => Opcode3 r1 r2 IInit -> Element rate
lfo_ opF =
    mkOpcode "lfo" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,itype) ->
                [ getConfUniv amp,  getConfUniv cps
                , getConfI itype ]



--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this
-- correct?

buzz :: Int -> Opcode3 r1 r2 KRate -> Element ARate
buzz ifn opF =
    mkOpcode "buzz" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh) ->
                [ getConfUniv xamp,  getConfUniv xcps
                , getConfK knh,      getConfI $ tablefn ifn ]

buzz_ :: Int -> Opcode4 r1 r2 KRate IInit -> Element ARate
buzz_ ifn opF =
    mkOpcode "buzz" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,iphs) ->
                [ getConfUniv xamp,  getConfUniv xcps
                , getConfK knh,      getConfI $ tablefn ifn
                , getConfI iphs ]


gbuzz :: Int -> Opcode5 r1 r2 KRate KRate KRate -> Element ARate
gbuzz ifn opF =
    mkOpcode "gbuzz" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,klh,kr) ->
                [ getConfUniv xamp,   getConfUniv xcps
                , getConfK knh,       getConfK klh
                , getConfK kr,        getConfI $ tablefn ifn ]

gbuzz_ :: Int -> Opcode6 r1 r2 KRate KRate KRate IInit -> Element ARate
gbuzz_ ifn opF = 
    mkOpcode "gbuzz" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,klh,kr,iphs) ->
                [ getConfUniv xamp,   getConfUniv xcps
                , getConfK knh,       getConfK klh
                , getConfK kr,        getConfI $ tablefn ifn
                , getConfI iphs ]

vco :: Int -> Opcode5 KRate KRate IInit KRate IInit -> Element ARate
vco ifn opF =
    mkOpcode "gbuzz" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kfqc,iwave,kpw,imaxd) ->
                [ getConfK kamp,          getConfK kfqc
                , getConfI iwave,         getConfK kpw
                , getConfI $ tablefn ifn, getConfI imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: String -> Opcode3 KRate KRate KRate -> Element ARate
adsyn ss opF =
    mkOpcode "adsyn" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamod,kfmod,ksmod) ->
                [ getConfK kamod,   getConfK kfmod
                , getConfK ksmod,   getConfI $ filecode ss ]


adsynt :: Int -> Int -> Int -> Opcode3 KRate KRate IInit  -> Element ARate
adsynt iwfn ifreqfn iampfn opF =
    mkOpcode "adsynt" inspec [iwfn,ifreqfn,iampfn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kcps,icnt) ->
                [ getConfK kamp,              getConfK kcps
                , getConfI $ tablefn iwfn,    getConfI $ tablefn ifreqfn
                , getConfI $ tablefn iampfn,  getConfI icnt ]


adsynt_ :: Int -> Int -> Int 
        -> Opcode4 KRate KRate IInit IInit 
        -> Element ARate
adsynt_ iwfn ifreqfn iampfn opF = 
    mkOpcode "adsynt" inspec [iwfn,ifreqfn,iampfn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kcps,icnt,iphs) ->
                [ getConfK kamp,              getConfK kcps
                , getConfI $ tablefn iwfn,    getConfI $ tablefn ifreqfn
                , getConfI $ tablefn iampfn,  getConfI icnt
                , getConfI iphs ]


hsboscil :: Int -> Int -> Opcode4 KRate KRate KRate IInit -> Element ARate
hsboscil iwfn ioctfn opF = 
    mkOpcode "hsboscil" inspec [iwfn,ioctfn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,ktone,kbrite,ibasfreq) ->
                [ getConfK kamp,            getConfK ktone
                , getConfK kbrite,          getConfI ibasfreq
                , getConfI $ tablefn iwfn,  getConfI $ tablefn ioctfn ]

hsboscil_ :: Int -> Int -> Opcode5 KRate KRate KRate IInit IInit -> Element ARate
hsboscil_ iwfn ioctfn opF = 
    mkOpcode "hsboscil" inspec [iwfn,ioctfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \(kamp,ktone,kbrite,ibasfreq,iphs) ->
                    [ getConfK kamp,            getConfK ktone
                    , getConfK kbrite,          getConfI ibasfreq
                    , getConfI $ tablefn iwfn,  getConfI $ tablefn ioctfn
                    , getConfI iphs ]


--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Int -> Opcode5 rate KRate KRate KRate KRate -> Element ARate
foscil ifn opF =
    mkOpcode "foscil" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfK kcar,     getConfK kmod
                , getConfK kndx,     getConfI $ tablefn ifn ]

foscil_ :: Int -> Opcode6 rate KRate KRate KRate KRate IInit -> Element ARate
foscil_ ifn opF  =
    mkOpcode "foscil" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx,iphs) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfK kcar,     getConfK kmod
                , getConfK kndx,     getConfI $ tablefn ifn
                , getConfI iphs ]

foscili :: Int -> Opcode5 rate KRate KRate KRate KRate -> Element ARate
foscili ifn opF =
    mkOpcode "foscili" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx) ->
                [ getConfUniv xamp,   getConfK kcps
                , getConfK kcar,      getConfK kmod
                , getConfK kndx,      getConfI $ tablefn ifn ]


foscili_ :: Int -> Opcode6 rate KRate KRate KRate KRate IInit -> Element ARate
foscili_ ifn opF =
    mkOpcode "foscil" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx,iphs) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfK kcar,     getConfK kmod
                , getConfK kndx,     getConfI $ tablefn ifn
                , getConfI iphs ]


fmvoice :: Int -> Int -> Int -> Int -> Int 
         -> Opcode6 KRate KRate KRate KRate KRate KRate
         -> Element ARate
fmvoice ifn1 ifn2 ifn3 ifn4 ivibfn opF =
    mkOpcode "fmvoice" inspec [ifn1, ifn2, ifn3, ifn4, ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kvowel, ktilt, kvibamt, kvibrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kvowel,          getConfK ktilt
                    , getConfK kvibamt,         getConfK kvibrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]

fmbell :: Int -> Int -> Int -> Int -> Int 
         -> Opcode6 KRate KRate KRate KRate KRate KRate
       -> Element ARate
fmbell ifn1 ifn2 ifn3 ifn4 ivibfn opF =
    mkOpcode "fmbell" inspec [ifn1, ifn2, ifn3, ifn4, ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kc1,             getConfK kc2
                    , getConfK kvdepth,         getConfK kvrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]

fmrhode :: Int -> Int -> Int -> Int -> Int 
        -> Opcode6 KRate KRate KRate KRate KRate KRate
        -> Element ARate
fmrhode ifn1 ifn2 ifn3 ifn4 ivibfn opF =
    mkOpcode "fmrhode" inspec [ifn1, ifn2, ifn3, ifn4, ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kc1,             getConfK kc2
                    , getConfK kvdepth,         getConfK kvrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]


fmwurlie :: Int -> Int -> Int -> Int -> Int 
         -> Opcode6 KRate KRate KRate KRate KRate KRate
         -> Element ARate
fmwurlie ifn1 ifn2 ifn3 ifn4 ivibfn opF =
    mkOpcode "fmwurlie" inspec [ifn1, ifn2, ifn3, ifn4, ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kc1,             getConfK kc2
                    , getConfK kvdepth,         getConfK kvrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]

fmmetal :: Int -> Int -> Int -> Int -> Int 
        -> Opcode6 KRate KRate KRate KRate KRate KRate
        -> Element ARate
fmmetal ifn1 ifn2 ifn3 ifn4 ivibfn opF =
    mkOpcode "fmmetal" inspec [ifn1, ifn2, ifn3, ifn4, ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kc1,             getConfK kc2
                    , getConfK kvdepth,         getConfK kvrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]

fmb3 :: Int -> Int -> Int -> Int -> Int 
     -> Opcode6 KRate KRate KRate KRate KRate KRate
     -> Element ARate
fmb3 ifn1 ifn2 ifn3 ifn4 ivibfn opF =
    mkOpcode "fmb3" inspec [ifn1, ifn2, ifn3, ifn4, ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kc1,             getConfK kc2
                    , getConfK kvdepth,         getConfK kvrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]


fmpercfl :: Int -> Int -> Int -> Int -> Int 
         -> Opcode6 KRate KRate KRate KRate KRate KRate        
         -> Element ARate
fmpercfl ifn1 ifn2 ifn3 ifn4 ivibfn opF = 
    mkOpcode "fmpercfl" inspec [ifn1,ifn2,ifn3,ifn4,ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kc1,             getConfK kc2
                    , getConfK kvdepth,         getConfK kvrate
                    , getConfI $ tablefn ifn1,  getConfI $ tablefn ifn2
                    , getConfI $ tablefn ifn3,  getConfI $ tablefn ifn4
                    , getConfI $ tablefn ivibfn ]

--------------------------------------------------------------------------------
-- Sample playback

-- Note - it seems idiomatic to want a stereo version will only
-- the mandatory args...


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil :: Int -> Opcode4 rate KRate IInit IInit
       -> Element ARate
loscil ifn opF =
    mkOpcode "loscil" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ibase,imod1) ->
                [ getConfUniv xamp,         getConfK kcps
                , getConfI $ tablefn ifn,   getConfI ibase
                , getConfI imod1 ]


-- | Stereo version of 'loscil'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil :: Int -> Opcode4 rate KRate IInit IInit
         -> Element ARate
biloscil ifn opF =
    mkOpcode "loscil" inspec [ifn] (Out2 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ibase,imod1) ->
                [ getConfUniv xamp,         getConfK kcps
                , getConfI $ tablefn ifn,   getConfI ibase
                , getConfI imod1 ]

-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil3 :: Int -> Opcode4 rate KRate IInit IInit -> Element ARate
loscil3 ifn opF =
    mkOpcode "loscil3" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ibase,imod1) ->
                [ getConfUniv xamp,         getConfK kcps
                , getConfI $ tablefn ifn,   getConfI ibase
                , getConfI imod1 ]


-- | Stereo version of 'loscil3'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil3 :: Int -> Opcode4 rate KRate IInit IInit -> Element ARate
biloscil3 ifn opF =
    mkOpcode "loscil3" inspec [ifn] (Out2 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ibase,imod1) ->
                [ getConfUniv xamp,         getConfK kcps
                , getConfI $ tablefn ifn,   getConfI ibase
                , getConfI  imod1 ]

lposcil :: Int -> Opcode4 KRate KRate KRate KRate -> Element ARate
lposcil ifn opF =
    mkOpcode "lposcil" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kfreqrat,kloop,kend) ->
                [ getConfK kamp,            getConfK kfreqrat
                , getConfK kloop,           getConfK kend
                , getConfI $ tablefn ifn ]

lposcil3 :: Int -> Opcode4 KRate KRate KRate KRate -> Element ARate
lposcil3 ifn opF =
    mkOpcode "lposcil3" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kfreqrat,kloop,kend) ->
                [ getConfK kamp,            getConfK kfreqrat
                , getConfK kloop,           getConfK kend
                , getConfI $ tablefn ifn ]



--------------------------------------------------------------------------------
-- Models and emulations

moog :: Int -> Int -> Int -> Opcode6 KRate KRate KRate KRate KRate KRate
     -> Element ARate
moog iafn iwfn ivfn opF =
    mkOpcode "moog" inspec [iafn, iwfn, ivfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kfiltq, kfiltrate, kvibf, kvamp ) ->
                    [ getConfK kamp,            getConfK kfreq
                    , getConfK kfiltq,          getConfK kfiltrate
                    , getConfK kvibf,           getConfK kvamp
                    , getConfI $ tablefn iafn,  getConfI $ tablefn iwfn
                    , getConfI $ tablefn ivfn ]


-- | idecay - default is 0
shaker :: Opcode6 KRate KRate KRate KRate KRate IInit
       -> Element ARate
shaker opF =
    mkOpcode "shaker" inspec [] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kbeans, kdamp, ktimes, idecay ) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfK kbeans,   getConfK kdamp
                      , getConfK ktimes,   getConfI idecay ]


marimba :: Int -> Int -> Opcode7 KRate KRate IInit IInit KRate KRate IInit
        -> Element ARate
marimba imp ivibfn opF =
    mkOpcode "marimba" inspec [imp,ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, ihrd, ipos, kvibf, kvamp, idec ) ->
                      [ getConfK kamp,          getConfK kfreq
                      , getConfI ihrd,          getConfI ipos
                      , getConfI $ tablefn imp, getConfK kvibf
                      , getConfK kvamp,         getConfI $ tablefn ivibfn
                      , getConfI idec ]


vibes :: Int -> Int -> Opcode7 KRate KRate IInit IInit KRate KRate IInit
      -> Element ARate
vibes imp ivibfn opF =
    mkOpcode "vibes" inspec [imp,ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, ihrd, ipos, kvibf, kvamp, idec ) ->
                      [ getConfK kamp,          getConfK kfreq
                      , getConfI ihrd,          getConfI ipos
                      , getConfI $ tablefn imp, getConfK kvibf
                      , getConfK kvamp,         getConfI $ tablefn ivibfn
                      , getConfI idec ]

-- | iminfreq - default 0
mandol :: Int -> Opcode7 KRate KRate KRate KRate KRate KRate IInit
       -> Element ARate
mandol ifn opF =
    mkOpcode "marimba" inspec [ifn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kpluck, kdetune, kgain
                 , ksize, iminfreq ) ->
                      [ getConfK kamp,          getConfK kfreq
                      , getConfK kpluck,        getConfK kdetune
                      , getConfK kgain,         getConfK ksize
                      , getConfI $ tablefn ifn, getConfI iminfreq ]


gogobel :: Int -> Opcode7 KRate KRate IInit IInit IInit KRate KRate
        -> Element ARate
gogobel ivibfn opF =
    mkOpcode "gogobel" inspec [ivibfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, ihrd, ipos, imp, kvibf, kvamp ) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfI ihrd,     getConfI ipos
                      , getConfI imp,      getConfK kvibf
                      , getConfK kvamp,    getConfI $ tablefn ivibfn ]

voice :: Int -> Int -> Opcode6 KRate KRate KRate KRate KRate KRate
      -> Element ARate
voice ifn ivfn opF =
    mkOpcode "voice" inspec [ifn,ivfn] (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kphoneme, kform, kvibf, kvamp ) ->
                      [ getConfK kamp,          getConfK kfreq
                      , getConfK kphoneme,      getConfK kform
                      , getConfK kvibf,         getConfK kvamp
                      , getConfI $ tablefn ifn, getConfI $ tablefn ivfn ]

lorenz :: Opcode8 KRate KRate KRate KRate IInit IInit IInit IInit
      -> Element ARate
lorenz opF =
    mkOpcode "lorenz" inspec [] (OutN A 3)
  where
    inspec = applyOpcode opF $ 
                \( ks, kr, kb, kh, ix, iy, iz, ivx ) ->
                      [ getConfK ks,       getConfK kr
                      , getConfK kb,       getConfK kh
                      , getConfI ix,       getConfI iy
                      , getConfI iz,       getConfI ivx ]



planet :: Opcode11 KRate KRate KRate IInit IInit 
                   IInit IInit IInit IInit IInit IInit
      -> Element ARate
planet opF =
    mkOpcode "planet" inspec [] (OutN A 3)
  where
    inspec = applyOpcode opF $ 
                \( kmass1, kmass2, ksep, ix, iy, iz
                 , ivx, ivy, ivz, idelta, ifriction ) ->
                      [ getConfK kmass1,   getConfK kmass2
                      , getConfK ksep,     getConfI ix
                      , getConfI iy,       getConfI iz
                      , getConfI ivx,      getConfI ivy
                      , getConfI ivz,      getConfI idelta
                      , getConfI ifriction ]



--------------------------------------------------------------------------------
-- Random noise generators


rand :: forall rate. (KA_Rate rate)
     => Opcode1 rate -> Element rate
rand opF =
    mkOpcode "rand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \amp ->
                [ getConfUniv amp ]

randh :: forall rate. (KA_Rate rate)
      => Opcode2 rate rate -> Element rate
randh opF =
    mkOpcode "rand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp, cps) ->
                [ getConfUniv amp, getConfUniv cps ]

randi :: forall rate. (KA_Rate rate)
      => Opcode2 rate rate -> Element rate
randi opF =
    mkOpcode "rand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp, cps) ->
                [ getConfUniv amp, getConfUniv cps ]

linrand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
linrand opF =
    mkOpcode "linrand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

trirand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
trirand opF =
    mkOpcode "trirand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

exprand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
exprand opF =
    mkOpcode "exprand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

bexprand :: forall rate. (Rate rate)
         => Opcode1 KRate -> Element rate
bexprand opF =
    mkOpcode "bexprand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

cauchy :: forall rate. (Rate rate)
       => Opcode1 KRate -> Element rate
cauchy opF =
    mkOpcode "cauchy" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \kalpha ->
                [ getConfK kalpha ]


pcauchy :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
pcauchy opF =
    mkOpcode "pcauchy" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \kalpha ->
                [ getConfK kalpha ]

poisson :: forall rate. (Rate rate)
       => Opcode1 KRate -> Element rate
poisson opF =
    mkOpcode "poisson" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \klambda ->
                  [ getConfK klambda ]

gauss :: forall rate. (Rate rate)
      => Opcode1 KRate -> Element rate
gauss opF =
    mkOpcode "gauss" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

weibull :: forall rate. (Rate rate)
        => Opcode2 KRate KRate -> Element rate
weibull opF =
    mkOpcode "weibull" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ksigma, ktau) ->
                [ getConfK ksigma, getConfK ktau ]

betarand :: forall rate. (Rate rate)
         => Opcode3 KRate KRate KRate -> Element rate
betarand opF = 
    mkOpcode "betarand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(krange,kalpha,kbeta) ->
                [ getConfK krange,  getConfK kalpha
                , getConfK kbeta ]

unirand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
unirand opF =
    mkOpcode "unirand" inspec [] (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
               [ getConfK krange ]


