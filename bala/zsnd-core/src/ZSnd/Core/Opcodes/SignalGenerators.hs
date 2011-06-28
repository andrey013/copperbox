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
  , LinesegConfig
  , LinesegOpcode
  , linseg

  , LinesegrConfig
  , LinesegrOpcode
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


import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.HighLevel
import ZSnd.Core.Inst.Index

--------------------------------------------------------------------------------
-- Linear and Exponential generators





line :: forall rate . (KA_Rate rate)
     => Opcode3 IRate IRate IRate -> Element rate
line opF = 
    mkElement "line" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib) -> 
                [ getConfI ia, getConfI idur, getConfI ib ] 
    



expon :: forall rate. (KA_Rate rate)
      => Opcode3 IRate IRate IRate -> Element rate
expon opF = 
    mkElement "expon" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idur,ib) -> 
                [ getConfI ia, getConfI idur, getConfI ib ]


-- Complex input signatures are a pain...

type LinesegConfig = ( Conf IRate, Conf IRate
                     , Conf IRate, [(Conf IRate, Conf IRate)] )
                      
type LinesegOpcode = ElemRef -> PortDict -> Either FailMsg LinesegConfig


linseg :: forall rate. (KA_Rate rate)
       => LinesegOpcode -> Element rate
linseg opF = 
    mkElement "linseg" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                in (getConfI ia : getConfI idur : getConfI ib : rest)
    

type LinesegrConfig = ( Conf IRate, Conf IRate
                      , Conf IRate, [(Conf IRate, Conf IRate)]
                      , Conf IRate, Conf IRate )
                      
type LinesegrOpcode = ElemRef -> PortDict -> Either FailMsg LinesegrConfig


linsegr :: forall rate. (KA_Rate rate)
        => LinesegrOpcode -> Element rate
linsegr opF = 
    mkElement "linsegr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs, irel, iz) ->
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                    end  = [getConfI irel, getConfI iz]
                in (getConfI ia : getConfI idur : getConfI ib : rest ++ end)


type ExpsegConfig = ( Conf IRate, Conf IRate
                    , Conf IRate, [(Conf IRate, Conf IRate)])
                      
type ExpsegOpcode = ElemRef -> PortDict -> Either FailMsg ExpsegConfig


expseg :: forall rate. (KA_Rate rate )
       => ExpsegOpcode -> Element rate
expseg opF = 
    mkElement "expseg" inspec 
                       (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                in (getConfI ia : getConfI idur : getConfI ib : rest)

type ExpsegrConfig = ( Conf IRate, Conf IRate
                     , Conf IRate, [(Conf IRate, Conf IRate)]
                     , Conf IRate, Conf IRate )
                      
type ExpsegrOpcode = ElemRef -> PortDict -> Either FailMsg ExpsegrConfig

expsegr :: forall rate. (KA_Rate rate)
        => ExpsegrOpcode -> Element rate
expsegr opF = 
    mkElement "linsegr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia, idur, ib, xs, irel, iz) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                    end  = [getConfI irel, getConfI iz]
                in (getConfI ia : getConfI idur : getConfI ib : rest ++ end)


type ExpsegaConfig = ( Conf IRate, Conf IRate
                     , Conf IRate, [(Conf IRate, Conf IRate)])
                      
type ExpsegaOpcode = ElemRef -> PortDict -> Either FailMsg ExpsegaConfig


expsega :: ExpsegaOpcode -> Element ARate
expsega opF = 
    mkElement "expsega" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(ia,idur,ib,xs) -> 
                let rest = concatMap (\(a,b) -> [getConfI a, getConfI b]) xs
                in (getConfI ia : getConfI idur : getConfI ib : rest)

adsr :: forall rate. (KA_Rate rate)
     => Opcode4 IRate IRate IRate IRate -> Element rate
adsr opF =
    mkElement "adsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,   getConfI idec
                , getConfI isl,  getConfI ir ]

adsr_ :: forall rate. (KA_Rate rate)
      => Opcode5 IRate IRate IRate IRate IRate -> Element rate
adsr_ opF =
    mkElement "adsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) ->
               [ getConfI ia,   getConfI idec
               , getConfI isl,  getConfI ir
               , getConfI idel ]


madsr :: forall rate. (KA_Rate rate)
      => Opcode4 IRate IRate IRate IRate -> Element rate
madsr opF = 
    mkElement "madsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir ]

madsr_ :: forall rate. (KA_Rate rate)
       => Opcode5 IRate IRate IRate IRate IRate -> Element rate
madsr_ opF =
    mkElement "madsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) -> 
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir
                , getConfI idel ]

xadsr :: forall rate. (KA_Rate rate)
      => Opcode4 IRate IRate IRate IRate -> Element rate
xadsr opF =
    mkElement "xadsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir ]


xadsr_ :: forall rate. (KA_Rate rate)
       => Opcode5 IRate IRate IRate IRate IRate -> Element rate
xadsr_ opF =
    mkElement "xadsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) ->
                [ getConfI ia,    getConfI idec
                , getConfI isl,   getConfI ir
                , getConfI idel ]

mxadsr :: forall rate. (KA_Rate rate)
       => Opcode4 IRate IRate IRate IRate -> Element rate
mxadsr opF =
    mkElement "mxadsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir) ->
                [ getConfI ia,   getConfI idec
                , getConfI isl,  getConfI ir ]

mxadsr_ :: forall rate. (KA_Rate rate)
        => Opcode5 IRate IRate IRate IRate IRate -> Element rate
mxadsr_ opF =
    mkElement "mxadsr" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ia,idec,isl,ir,idel) ->
                [ getConfI ia,   getConfI idec
                , getConfI isl,  getConfI ir
                , getConfI idel ]


--------------------------------------------------------------------------------
-- Table access

table :: forall rate1 rate. (Rate rate)
      => Opcode2 rate1 IRate -> Element rate
table opF =
    mkElement "table" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx, ifn) ->
                [ getConfUniv ndx, getConfI ifn ]

table_ :: forall rate1 rate. (Rate rate)
       => Opcode5 rate1 IRate IRate IRate IRate -> Element rate
table_ opF =
    mkElement "table" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ifn,ixmode,ixoff,ixwrap) ->
                [ getConfUniv ndx,    getConfI ifn
                , getConfI ixmode,    getConfI ixoff
                , getConfI ixwrap ]

tablei :: forall rate1 rate. (Rate rate)
       => Opcode2 rate1 IRate -> Element rate
tablei opF =
    mkElement "tablei" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ifn) ->
                [ getConfUniv ndx, getConfI ifn ]

tablei_ :: forall rate1 rate. (Rate rate)
        => Opcode5 rate1 IRate IRate IRate IRate -> Element rate
tablei_ opF =
    mkElement "tablei" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ifn,ixmode,ixoff,ixwrap) ->
                [ getConfUniv ndx,   getConfI ifn
                , getConfI ixmode,   getConfI ixoff
                , getConfI ixwrap ]



table3 :: forall rate1 rate. (Rate rate)
       => Opcode2 rate1 IRate -> Element rate
table3 opF =
    mkElement "table" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ifn) ->
             [ getConfUniv ndx, getConfI ifn ]


table3_ :: forall rate1 rate. (Rate rate)
        => Opcode5 rate1 IRate IRate IRate IRate -> Element rate
table3_ opF =
    mkElement "table3" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ndx,ifn,ixmode,ixoff,ixwrap) ->
                [ getConfUniv ndx,   getConfI ifn
                , getConfI ixmode,   getConfI ixoff
                , getConfI ixwrap ]


oscil1 :: Opcode4 IRate KRate IRate IRate -> Element KRate
oscil1 opF =
    mkElement "oscil1" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(idel,kamp,idur,ifn) ->
               [ getConfI idel,   getConfK kamp
               , getConfI idur,   getConfI ifn ]

oscil1i :: Opcode4 IRate KRate IRate IRate -> Element KRate
oscil1i opF =
    mkElement "oscil1i" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(idel,kamp,idur,ifn) ->
                [ getConfI idel,  getConfK kamp
                , getConfI idur,  getConfI ifn ]

osciln :: Opcode4 KRate IRate IRate IRate -> Element KRate
osciln opF =
    mkElement "osciln" inspec (Out1 K)
  where
    inspec = applyOpcode opF $ \(kamp,ifrq,ifn,itimes) -> 
                [ getConfK kamp,   getConfI ifrq
                , getConfI ifn,    getConfI itimes ]

--------------------------------------------------------------------------------
-- Phasors

-- class CPhasor rate where


phasor :: forall rate1 rate. (KA_Rate rate)
       => Opcode1 rate1 -> Element rate
phasor opF =
    mkElement "phasor" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \cps ->
                [ getConfUniv cps ]


phasor_ :: forall rate1 rate. (KA_Rate rate)
        => Opcode2 rate1 IRate -> Element rate
phasor_ opF =
    mkElement "phasor" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(cps,iphs) ->
                [ getConfUniv cps, getConfI iphs ]


phasorbnk :: forall rate1 rate. (KA_Rate rate)
          => Opcode3 rate1 KRate IRate -> Element rate
phasorbnk opF =
    mkElement "phasorbnk" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(cps,kindx,icnt) ->
                [ getConfUniv cps,  getConfK kindx
                , getConfI icnt ]


phasorbnk_  :: forall rate1 rate. (KA_Rate rate)
            => Opcode4 rate1 KRate IRate IRate -> Element rate
phasorbnk_ opF = 
    mkElement "phasorbnk" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(cps,kindx,icnt,iphs) ->
                [ getConfUniv cps,  getConfK kindx
                , getConfI icnt,    getConfI iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

-- | Note for A rate, cps can seemingly be any type.
-- 
oscil :: forall r1 r2 rate. (KA_Rate rate)
      => Opcode3 r1 r2 IRate -> Element rate
oscil opF = 
    mkElement "oscil" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ifn) ->
                [ getConfUniv amp,  getConfUniv cps
                , getConfI ifn ]


oscil_ :: forall r1 r2 rate. (KA_Rate rate)
       => Opcode4 r1 r2 IRate  IRate -> Element rate
oscil_ opF =
    mkElement "oscil" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ifn,iphs) ->
                [ getConfUniv amp,  getConfUniv cps
                , getConfI ifn,     getConfI iphs ]

oscili :: forall r1 r2 rate. (KA_Rate rate)
       => Opcode3 r1 r2 IRate -> Element rate
oscili opF = 
    mkElement "oscili" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ifn) ->
                [ getConfUniv amp,   getConfUniv cps
                , getConfI ifn ]

oscili_ :: forall r1 r2 rate. (KA_Rate rate)
        => Opcode4 r1 r2 IRate IRate -> Element rate
oscili_ opF =
    mkElement "oscili" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ifn,iphs) ->
                [ getConfUniv amp,   getConfUniv cps
                , getConfI ifn,      getConfI iphs ]

oscil3 :: forall r1 r2 rate. (KA_Rate rate)
       => Opcode3 r1 r2 IRate -> Element rate
oscil3 opF =
    mkElement "oscil3" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ifn) ->
                [ getConfUniv amp,   getConfUniv cps
                , getConfI ifn ]

oscil3_ :: forall r1 r2 rate. (KA_Rate rate)
        => Opcode4 r1 r2 IRate IRate -> Element rate
oscil3_ opF = 
    mkElement "oscil3" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ifn,iphs) ->
                [ getConfUniv amp,   getConfUniv cps
                , getConfI ifn,      getConfI iphs ]

poscil :: forall r1 r2 rate. (KA_Rate rate)
       => Opcode3 r1 r2 IRate -> Element rate
poscil opF = 
    mkElement "poscil" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ift) ->
                [ getConfUniv amp,   getConfUniv cps
                , getConfI ift ]

poscil_ :: forall r1 r2 rate. (KA_Rate rate)
        => Opcode4 r1 r2 IRate IRate -> Element rate
poscil_ opF = 
    mkElement "poscil" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ift,iphs) ->
                [ getConfUniv amp,   getConfUniv cps
                , getConfI ift,      getConfI iphs ]


poscil3 :: forall r1 r2 rate. (KA_Rate rate)
        => Opcode3 r1 r2 IRate -> Element rate
poscil3 opF = 
    mkElement "poscil3" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ift) ->
                [ getConfUniv amp,  getConfUniv cps
                , getConfI ift ]


poscil3_  :: forall r1 r2 rate. (KA_Rate rate)
          => Opcode4 r1 r2 IRate IRate -> Element rate
poscil3_ opF = 
    mkElement "poscil3" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,ift,iphs) ->
                [ getConfUniv amp,  getConfUniv cps
                , getConfI ift,     getConfI iphs ]


lfo :: forall r1 r2 rate. (KA_Rate rate)
    => Opcode2 r1 r2 -> Element rate
lfo opF = 
    mkElement "lfo" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps) ->
                [ getConfUniv amp,  getConfUniv cps]


lfo_ :: forall r1 r2 rate. (KA_Rate rate)
     => Opcode3 r1 r2 IRate -> Element rate
lfo_ opF =
    mkElement "lfo" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp,cps,itype) ->
                [ getConfUniv amp,  getConfUniv cps
                , getConfI itype ]



--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this
-- correct?

buzz :: Opcode4 r1 r2 KRate IRate -> Element ARate
buzz opF =
    mkElement "buzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,ifn) ->
                [ getConfUniv xamp,  getConfUniv xcps
                , getConfK knh,      getConfI ifn ]

buzz_ :: Opcode5 r1 r2 KRate IRate IRate -> Element ARate
buzz_ opF =
    mkElement "buzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,ifn,iphs) ->
                [ getConfUniv xamp,  getConfUniv xcps
                , getConfK knh,      getConfI ifn
                , getConfI iphs ]


gbuzz :: Opcode6 r1 r2 KRate KRate KRate IRate -> Element ARate
gbuzz opF =
    mkElement "gbuzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,klh,kr,ifn) ->
                [ getConfUniv xamp,   getConfUniv xcps
                , getConfK knh,       getConfK klh
                , getConfK kr,        getConfI ifn ]

gbuzz_ :: Opcode7 r1 r2 KRate KRate KRate IRate IRate -> Element ARate
gbuzz_ opF = 
    mkElement "gbuzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,xcps,knh,klh,kr,ifn,iphs) ->
                [ getConfUniv xamp,   getConfUniv xcps
                , getConfK knh,       getConfK klh
                , getConfK kr,        getConfI ifn
                , getConfI iphs ]

vco :: Opcode6 KRate KRate IRate KRate IRate IRate -> Element ARate
vco opF =
    mkElement "gbuzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kfqc,iwave,kpw,ifn,imaxd) ->
                [ getConfK kamp,    getConfK kfqc
                , getConfI iwave,   getConfK kpw
                , getConfI ifn,     getConfI imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Opcode4 KRate KRate KRate IRate -> Element ARate
adsyn opF =
    mkElement "adsyn" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamod,kfmod,ksmod,ifilcod) ->
                [ getConfK kamod,   getConfK kfmod
                , getConfK ksmod,   getConfI ifilcod ]


adsynt :: Opcode6 KRate KRate IRate IRate IRate IRate -> Element ARate
adsynt opF =
    mkElement "adsynt" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kcps,iwfn,ifreqfn,iampfn,icnt) ->
                [ getConfK kamp,     getConfK kcps
                , getConfI iwfn,     getConfI ifreqfn
                , getConfI iampfn,   getConfI icnt ]


adsynt_ :: Opcode7 KRate KRate IRate IRate IRate IRate IRate -> Element ARate
adsynt_  opF = 
    mkElement "adsynt" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kcps,iwfn,ifreqfn,iampfn,icnt,iphs) ->
                [ getConfK kamp,     getConfK kcps
                , getConfI iwfn,     getConfI ifreqfn
                , getConfI iampfn,   getConfI icnt
                , getConfI iphs ]


hsboscil :: Opcode6 KRate KRate KRate IRate IRate IRate -> Element ARate
hsboscil opF = 
    mkElement "hsboscil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,ktone,kbrite,ibasfreq,iwfn,ioctfn) ->
                [ getConfK kamp,     getConfK ktone
                , getConfK kbrite,   getConfI ibasfreq
                , getConfI iwfn,     getConfI ioctfn ]

hsboscil_ :: Opcode7 KRate KRate KRate IRate IRate IRate IRate -> Element ARate
hsboscil_ opF = 
    mkElement "hsboscil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \(kamp,ktone,kbrite,ibasfreq,iwfn,ioctfn,iphs) ->
                    [ getConfK kamp,     getConfK ktone
                    , getConfK kbrite,   getConfI ibasfreq
                    , getConfI iwfn,     getConfI ioctfn
                    , getConfI iphs ]


--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Opcode6 rate KRate KRate KRate KRate IRate -> Element ARate
foscil opF =
    mkElement "foscil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx,ifn) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfK kcar,     getConfK kmod
                , getConfK kndx,     getConfI ifn ]

foscil_ :: Opcode7 rate KRate KRate KRate KRate IRate IRate -> Element ARate
foscil_ opF  =
    mkElement "foscil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx,ifn,iphs) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfK kcar,     getConfK kmod
                , getConfK kndx,     getConfI ifn
                , getConfI iphs ]

foscili :: Opcode6 rate KRate KRate KRate KRate IRate -> Element ARate
foscili opF =
    mkElement "foscili" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx,ifn) ->
                [ getConfUniv xamp,   getConfK kcps
                , getConfK kcar,      getConfK kmod
                , getConfK kndx,      getConfI ifn ]


foscili_ :: Opcode7 rate KRate KRate KRate KRate IRate IRate -> Element ARate
foscili_ opF =
    mkElement "foscil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,kcar,kmod,kndx,ifn,iphs) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfK kcar,     getConfK kmod
                , getConfK kndx,     getConfI ifn
                , getConfI iphs ]


fmvoice :: Opcode11 KRate KRate KRate KRate KRate KRate
                    IRate IRate IRate IRate IRate
        -> Element ARate
fmvoice opF =
    mkElement "fmvoice" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kvowel, ktilt, kvibamt
                 , kvibrate, ifn1, ifn2, ifn3, ifn4, ivibfn ) ->
                      [ getConfK kamp,      getConfK kfreq
                      , getConfK kvowel,    getConfK ktilt
                      , getConfK kvibamt,   getConfK kvibrate
                      , getConfI ifn1,      getConfI ifn2
                      , getConfI ifn3,      getConfI ifn4
                      , getConfI ivibfn ]

fmbell :: Opcode11 KRate KRate KRate KRate KRate KRate
                   IRate IRate IRate IRate IRate
       -> Element ARate
fmbell opF =
    mkElement "fmbell" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate
                 , ifn1, ifn2, ifn3, ifn4, ivibfn ) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfK kc1,      getConfK kc2
                      , getConfK kvdepth,  getConfK kvrate
                      , getConfI ifn1,     getConfI ifn2
                      , getConfI ifn3,     getConfI ifn4
                      , getConfI ivibfn ]

fmrhode :: Opcode11 KRate KRate KRate KRate KRate KRate
                    IRate IRate IRate IRate IRate
        -> Element ARate
fmrhode opF =
    mkElement "fmrhode" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate
                 , ifn1, ifn2, ifn3, ifn4, ivibfn) ->
                      [ getConfK kamp,      getConfK kfreq
                      , getConfK kc1,       getConfK kc2
                      , getConfK kvdepth,   getConfK kvrate
                      , getConfI ifn1,      getConfI ifn2
                      , getConfI ifn3,      getConfI ifn4
                      , getConfI ivibfn ]


fmwurlie :: Opcode11 KRate KRate KRate KRate KRate KRate
                     IRate IRate IRate IRate IRate
         -> Element ARate
fmwurlie opF =
    mkElement "fmwurlie" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate 
                 , ifn1, ifn2, ifn3, ifn4, ivibfn) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfK kc1,      getConfK kc2
                      , getConfK kvdepth,  getConfK kvrate
                      , getConfI ifn1,     getConfI ifn2
                      , getConfI ifn3,     getConfI ifn4
                      , getConfI ivibfn ]

fmmetal :: Opcode11 KRate KRate KRate KRate KRate KRate
                    IRate IRate IRate IRate IRate
        -> Element ARate
fmmetal opF =
    mkElement "fmmetal" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate 
                 , ifn1, ifn2, ifn3, ifn4, ivibfn) ->
                      [ getConfK kamp,      getConfK kfreq
                      , getConfK kc1,       getConfK kc2
                      , getConfK kvdepth,   getConfK kvrate
                      , getConfI ifn1,      getConfI ifn2
                      , getConfI ifn3,      getConfI ifn4
                      , getConfI ivibfn ]

fmb3 :: Opcode11 KRate KRate KRate KRate KRate KRate
                 IRate IRate IRate IRate IRate
     -> Element ARate
fmb3 opF =
    mkElement "fmb3" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate
                 , ifn1, ifn2, ifn3, ifn4, ivibfn ) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfK kc1,      getConfK kc2
                      , getConfK kvdepth,  getConfK kvrate
                      , getConfI ifn1,     getConfI ifn2
                      , getConfI ifn3,     getConfI ifn4
                      , getConfI ivibfn ]


fmpercfl :: Opcode11 KRate KRate KRate KRate KRate KRate
                     IRate IRate IRate IRate IRate
         -> Element ARate
fmpercfl opF = 
    mkElement "fmpercfl" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kc1, kc2, kvdepth, kvrate
                 , ifn1, ifn2, ifn3, ifn4, ivibfn ) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfK kc1,      getConfK kc2
                      , getConfK kvdepth,  getConfK kvrate
                      , getConfI ifn1,     getConfI ifn2
                      , getConfI ifn3,     getConfI ifn4
                      , getConfI ivibfn ]

--------------------------------------------------------------------------------
-- Sample playback

-- Note - it seems idiomatic to want a stereo version will only
-- the mandatory args...


-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil :: Opcode5 rate KRate IRate IRate IRate
       -> Element ARate
loscil opF =
    mkElement "loscil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ifn,ibase,imod1) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfI ifn,      getConfI ibase
                , getConfI imod1 ]


-- | Stereo version of 'loscil'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil :: Opcode5 rate KRate IRate IRate IRate
         -> Element ARate
biloscil opF =
    mkElement "loscil" inspec (Out2 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ifn,ibase,imod1) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfI ifn,      getConfI ibase
                , getConfI imod1 ]

-- | Note - use 1 for ibase and kcps if the frequency is not known.
--
loscil3 :: Opcode5 rate KRate IRate IRate IRate -> Element ARate
loscil3 opF =
    mkElement "loscil3" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ifn,ibase,imod1) ->
                [ getConfUniv xamp,   getConfK kcps
                , getConfI ifn,       getConfI ibase
                , getConfI imod1 ]


-- | Stereo version of 'loscil3'.
--
--Note - use 1 for ibase and kcps if the frequency is not known.
--
biloscil3 :: Opcode5 rate KRate IRate IRate IRate -> Element ARate
biloscil3 opF =
    mkElement "loscil3" inspec (Out2 A)
  where
    inspec = applyOpcode opF $ \(xamp,kcps,ifn,ibase,imod1) ->
                [ getConfUniv xamp,  getConfK kcps
                , getConfI ifn,      getConfI ibase
                , getConfI  imod1 ]

lposcil :: Opcode5 KRate KRate KRate KRate IRate -> Element ARate
lposcil opF =
    mkElement "lposcil" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kfreqrat,kloop,kend,ift) ->
                [ getConfK kamp,    getConfK kfreqrat
                , getConfK kloop,   getConfK kend
                , getConfI ift ]

lposcil3 :: Opcode5 KRate KRate KRate KRate IRate -> Element ARate
lposcil3 opF =
    mkElement "lposcil3" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(kamp,kfreqrat,kloop,kend,ift) ->
                [ getConfK kamp,   getConfK kfreqrat
                , getConfK kloop,  getConfK kend
                , getConfI ift ]



--------------------------------------------------------------------------------
-- Models and emulations

moog :: Opcode9 KRate KRate KRate KRate KRate KRate IRate IRate IRate
     -> Element ARate
moog opF =
    mkElement "moog" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ 
                \( kamp, kfreq, kfiltq, kfiltrate, kvibf 
                 , kvamp, iafn, iwfn, ivfn ) ->
                      [ getConfK kamp,     getConfK kfreq
                      , getConfK kfiltq,   getConfK kfiltrate
                      , getConfK kvibf,    getConfK kvamp
                      , getConfI iafn,     getConfI iwfn
                      , getConfI ivfn ]

--------------------------------------------------------------------------------
-- Random noise generators


rand :: forall rate. (KA_Rate rate)
     => Opcode1 rate -> Element rate
rand opF =
    mkElement "rand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \amp ->
                [ getConfUniv amp ]

randh :: forall rate. (KA_Rate rate)
      => Opcode2 rate rate -> Element rate
randh opF =
    mkElement "rand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp, cps) ->
                [ getConfUniv amp, getConfUniv cps ]

randi :: forall rate. (KA_Rate rate)
      => Opcode2 rate rate -> Element rate
randi opF =
    mkElement "rand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(amp, cps) ->
                [ getConfUniv amp, getConfUniv cps ]

linrand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
linrand opF =
    mkElement "linrand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

trirand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
trirand opF =
    mkElement "trirand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

exprand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
exprand opF =
    mkElement "exprand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

bexprand :: forall rate. (Rate rate)
         => Opcode1 KRate -> Element rate
bexprand opF =
    mkElement "bexprand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

cauchy :: forall rate. (Rate rate)
       => Opcode1 KRate -> Element rate
cauchy opF =
    mkElement "cauchy" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \kalpha ->
                [ getConfK kalpha ]


pcauchy :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
pcauchy opF =
    mkElement "pcauchy" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \kalpha ->
                [ getConfK kalpha ]

poisson :: forall rate. (Rate rate)
       => Opcode1 KRate -> Element rate
poisson opF =
    mkElement "poisson" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \klambda ->
                  [ getConfK klambda ]

gauss :: forall rate. (Rate rate)
      => Opcode1 KRate -> Element rate
gauss opF =
    mkElement "gauss" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
                [ getConfK krange ]

weibull :: forall rate. (Rate rate)
        => Opcode2 KRate KRate -> Element rate
weibull opF =
    mkElement "weibull" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(ksigma, ktau) ->
                [ getConfK ksigma, getConfK ktau ]

betarand :: forall rate. (Rate rate)
         => Opcode3 KRate KRate KRate -> Element rate
betarand opF = 
    mkElement "betarand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(krange,kalpha,kbeta) ->
                [ getConfK krange,  getConfK kalpha
                , getConfK kbeta ]

unirand :: forall rate. (Rate rate)
        => Opcode1 KRate -> Element rate
unirand opF =
    mkElement "unirand" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \krange ->
               [ getConfK krange ]


