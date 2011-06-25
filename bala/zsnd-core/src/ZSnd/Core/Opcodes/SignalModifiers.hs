{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.SignalModifiers
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Signal modifying opcodes.
-- 
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.SignalModifiers
  (

  -- * Standard filters
    portmentok  -- Csound portk
  , portmento   -- Csound port
  , tonek
  , tone
  , atonek
  , atone
  , resonk
  , reson
  , aresonk
  , areson
  , tonex
  , atonex
  , resonx
  , resonr
  , resonz
  , resony
  , lowres
  , lowresx
  , vlowres

  -- * Specialized filters
  , nlfilt
  , pareq
  , dcblock
  , dcblock_

  -- * Envelope modifiers
  , linen
  , linenr
  , envlpx
  , envlpxr

  -- * Amplitude modifiers
  , rms
  , gain
  , balance
  , dam

  -- * signal limiters
  , wrap
  , mirror
  , limit

  -- * Delay
  , delayr
  , delayw
  , delay
  , delay1
  , deltap
  , deltapi
  , deltapn
  , deltap3
  , multitap
  , vdelay
  , vdelay3

  -- * Reverbaration
  , reverb
  , reverb2
  , nreverb
  , comb
  , alpass
  , nestedap

  -- * Panning ans spatialization
  , pan
  , pan2
  , locsig
  , locsig2

  ) where


import ZSnd.Core.CsoundInst
import ZSnd.Core.Inst.Click

--------------------------------------------------------------------------------
-- Standard filters

-- | Note this is @portk@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmentok :: Conf KRate -> Conf KRate -> Element KRate
portmentok ksig khtim = 
    mkElement "portk" [ getConfK ksig, getConfK khtim ]
                      (Out1 K)


-- | Note this is @port@ in Csound.
--
-- @port@ has special meaning in ZSnd, so it is not used here.
--
portmento :: Conf KRate -> Conf IRate -> Element KRate
portmento ksig ihtim = 
    mkElement "port" [ getConfK ksig, getConfI ihtim ] (Out1 K)

tonek :: Conf KRate -> Conf KRate -> Element KRate
tonek ksig khp = 
    mkElement "tonek" [ getConfK ksig, getConfK khp ] (Out1 K)

tone :: Conf ARate -> Conf KRate -> Element ARate
tone asig khp = 
    mkElement "tone" [ getConfA asig, getConfK khp ] (Out1 A)


atonek :: Conf KRate -> Conf KRate -> Element KRate
atonek ksig khp = 
    mkElement "atonek" [ getConfK ksig, getConfK khp ] (Out1 K)

atone :: Conf ARate -> Conf KRate -> Element ARate
atone asig khp = 
    mkElement "atone" [ getConfA asig, getConfK khp ] (Out1 A)

resonk :: Conf KRate -> Conf KRate -> Conf KRate -> Element KRate
resonk ksig kcf kbw = 
    mkElement "resonk" [ getConfK ksig, getConfK kcf, getConfK kbw ] (Out1 K)

reson :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
reson asig kcf kbw = 
    mkElement "reson" [ getConfA asig, getConfK kcf, getConfK kbw ] (Out1 A)


aresonk :: Conf KRate -> Conf KRate -> Conf KRate -> Element KRate
aresonk ksig kcf kbw = 
    mkElement "aresonk" [ getConfK ksig, getConfK kcf, getConfK kbw ] (Out1 K)

areson :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
areson asig kcf kbw = 
    mkElement "areson" [ getConfA asig, getConfK kcf, getConfK kbw ] (Out1 A)


tonex :: Conf ARate -> Conf KRate -> Element ARate
tonex asig khp = 
    mkElement "tonex" [ getConfA asig, getConfK khp ] (Out1 A)

atonex :: Conf ARate -> Conf KRate -> Element ARate
atonex asig khp = 
    mkElement "atonex" [ getConfA asig, getConfK khp ] (Out1 A)


resonx :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
resonx asig kcf kbw = 
    mkElement "resonx" [ getConfA asig, getConfK kcf, getConfK kbw ] (Out1 A)

resonr :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
resonr asig kcf kbw = 
    mkElement "resonr" [ getConfA asig, getConfK kcf, getConfK kbw ] (Out1 A)

resonz :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
resonz asig kcf kbw = 
    mkElement "resonz" [ getConfA asig, getConfK kcf, getConfK kbw ]
                       (Out1 A)

resony :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
resony asig kcf kbw = 
    mkElement "resony" [ getConfA asig, getConfK kcf, getConfK kbw ]
                       (Out1 A)

lowres :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
lowres asig kcutoff kreson = 
    mkElement "lowres" [ getConfA asig, getConfK kcutoff, getConfK kreson ]
                       (Out1 A)

lowresx :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
lowresx asig kcutoff kreson = 
    mkElement "lowresx" [ getConfA asig, getConfK kcutoff, getConfK kreson ]
                        (Out1 A)

vlowres :: Conf ARate -> Conf KRate -> Conf KRate -> Conf IRate -> Conf KRate 
        -> Element ARate
vlowres asig kfco kres iord ksep = 
    mkElement "vlowres" [ getConfA asig, getConfK kfco, getConfK kres
                        , getConfI iord, getConfK ksep ]
                        (Out1 A)


--------------------------------------------------------------------------------
-- Specialized filters

nlfilt :: Conf ARate -> Conf KRate -> Conf KRate 
       -> Conf KRate -> Conf KRate -> Conf KRate
       -> Element ARate
nlfilt ain ka kb kd kL kC = 
    mkElement "nlfilt" [ getConfA ain, getConfK ka, getConfK kb
                       , getConfK kd,  getConfK kL, getConfK kC ]
                       (Out1 A)


pareq :: Conf ARate -> Conf KRate -> Conf IRate -> Conf IRate -> Conf IRate
      -> Element ARate
pareq asig kc iv iq imode = 
    mkElement "pareq" [ getConfA asig, getConfK kc, getConfI iv
                      , getConfI iq,   getConfI imode ]
                      (Out1 A)



dcblock :: Conf ARate -> Element ARate
dcblock asig = 
    mkElement "dcblock" [ getConfA asig ] (Out1 A)

dcblock_ :: Conf ARate -> Conf IRate -> Element ARate
dcblock_ asig ig = 
    mkElement "dcblock" [ getConfA asig, getConfI ig ] (Out1 A)


--------------------------------------------------------------------------------
-- Envelope modifiers

linen :: forall rate. (KA_Rate rate)
      => Conf rate -> Conf IRate -> Conf IRate -> Conf IRate 
      -> Element rate
linen amp irise idur idec = 
    mkElement "linen" [ getConfUniv amp, getConfI irise
                      , getConfI idur, getConfI idec ]
                      (Out1 $ dataRate (undefined::rate))

linenr :: forall rate. (KA_Rate rate)
       => Conf rate -> Conf IRate -> Conf IRate -> Conf IRate 
       -> Element rate
linenr amp irise idec iatdec = 
    mkElement "linenr" [ getConfUniv amp, getConfI irise
                       , getConfI idec, getConfI iatdec ]
                       (Out1 $ dataRate (undefined::rate))

envlpx :: forall rate. (KA_Rate rate)
       => Conf rate -> Conf IRate -> Conf IRate -> Conf IRate -> Conf IRate 
       -> Conf IRate -> Conf IRate
       -> Element rate
envlpx amp irise idur idec ifn iatss iatdec = 
    mkElement "envlpx" [ getConfUniv amp, getConfI irise, getConfI idur
                       , getConfI idec, getConfI ifn, getConfI iatss
                       , getConfI iatdec ]
                       (Out1 $ dataRate (undefined::rate))

envlpxr :: forall rate. (KA_Rate rate)
        => Conf rate -> Conf IRate -> Conf IRate -> Conf IRate -> Conf IRate 
        -> Conf IRate -> Conf IRate
        -> Element rate
envlpxr amp irise idur idec ifn iatss iatdec = 
    mkElement "envlpxr" [ getConfUniv amp, getConfI irise, getConfI idur
                        , getConfI idec, getConfI ifn, getConfI iatss
                        , getConfI iatdec ]
                        (Out1 $ dataRate (undefined::rate))



--------------------------------------------------------------------------------
-- Amplitude modifiers

rms :: Conf ARate -> Element KRate
rms asig = 
    mkElement "rms" [ getConfA asig ] (Out1 K)

gain :: Conf ARate -> Conf KRate -> Element ARate
gain asig krms = 
    mkElement "gain" [ getConfA asig, getConfK krms ] (Out1 A)

balance :: Conf ARate -> Conf ARate -> Element ARate
balance asig acomp = 
    mkElement "balance" [ getConfA asig, getConfA acomp ] (Out1 A)


dam :: Conf ARate -> Conf KRate -> Conf IRate 
    -> Conf IRate -> Conf IRate -> Conf IRate
    -> Element ARate
dam asig kthreshold icomp1 icomp2 irtime iftime = 
    mkElement "dam" [ getConfA asig, getConfK kthreshold
                    , getConfI icomp1, getConfI icomp2
                    , getConfI irtime, getConfI iftime ]
                    (Out1 A)

--------------------------------------------------------------------------------
-- Signal limiters

wrap :: forall rate. (Rate rate)
     => Conf rate -> Conf rate -> Conf rate -> Element rate
wrap sig low high =
    mkElement "wrap" [ getConfUniv sig, getConfUniv low, getConfUniv high ]
                     (Out1 $ dataRate (undefined::rate))


mirror :: forall rate. (Rate rate)
       => Conf rate -> Conf rate -> Conf rate -> Element rate
mirror sig low high =
    mkElement "mirror" [ getConfUniv sig, getConfUniv low, getConfUniv high ]
                       (Out1 $ dataRate (undefined::rate))

limit :: forall rate. (Rate rate)
      => Conf rate -> Conf rate -> Conf rate -> Element rate
limit sig low high =
    mkElement "limit" [ getConfUniv sig, getConfUniv low, getConfUniv high ]
                      (Out1 $ dataRate (undefined::rate))

--------------------------------------------------------------------------------
-- Delay

delayr :: Conf IRate -> Element ARate
delayr idlt = 
    mkElement "delayr" [ getConfI idlt ] (Out1 A)


-- No answer ...
delayw :: Conf ARate -> Element rate
delayw asigar = 
    mkElement "delayw" [ getConfA asigar ] Out0



delay :: Conf ARate -> Conf IRate -> Element ARate
delay asig idlt = 
    mkElement "delay" [ getConfA asig, getConfI idlt ] (Out1 A)

delay1 :: Conf ARate -> Element ARate
delay1 asig = 
    mkElement "delay1" [ getConfA asig ] (Out1 A)


deltap :: Conf KRate -> Element ARate
deltap kdlt = 
    mkElement "deltap" [ getConfK kdlt ] (Out1 A)

-- | xdlt is seconds...
--
deltapi :: Conf a -> Element ARate
deltapi xdlt = 
    mkElement "deltapi" [ getConfUniv xdlt ] (Out1 A)

-- | xnumsamps is presumably an integer...
--
deltapn :: Conf a -> Element ARate
deltapn xnumsamps = 
    mkElement "deltapn" [ getConfUniv xnumsamps ] (Out1 A)


-- | xdlt is seconds...
--
deltap3 :: Conf a -> Element ARate
deltap3 xdlt = 
    mkElement "deltap3" [ getConfUniv xdlt ] (Out1 A)


multitap :: Conf ARate -> [(Conf IRate, Conf IRate)] -> Element ARate
multitap asig xs = 
    mkElement "multitap" (getConfA asig : concatMap fn xs) (Out1 A)
  where
    fn (a,b) = [ getConfI a, getConfI b ]


vdelay :: Conf ARate -> Conf ARate -> Conf IRate -> Element ARate
vdelay asig adel imaxdel = 
    mkElement "vdelay" [ getConfA asig, getConfA adel, getConfI imaxdel ]
                       (Out1 A)

vdelay3 :: Conf ARate -> Conf ARate -> Conf IRate -> Element ARate
vdelay3 asig adel imaxdel = 
    mkElement "vdelay3" [ getConfA asig, getConfA adel, getConfI imaxdel ]
                        (Out1 A)

--------------------------------------------------------------------------------
-- Reverberation

reverb :: Conf ARate -> Conf KRate -> Element ARate
reverb asig kvrt = 
    mkElement "reverb" [ getConfA asig, getConfK kvrt ] (Out1 A)

reverb2 :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
reverb2 asig ktime khdif = 
    mkElement "reverb2" [ getConfA asig, getConfK ktime, getConfK khdif ]
                        (Out1 A)

nreverb :: Conf ARate -> Conf KRate -> Conf KRate -> Element ARate
nreverb asig ktime khdif = 
    mkElement "nreverb" [ getConfA asig, getConfK ktime, getConfK khdif ]
                        (Out1 A)

comb :: Conf ARate -> Conf KRate -> Conf IRate -> Element ARate
comb asig kvrt ilpt = 
    mkElement "comb" [ getConfA asig, getConfK kvrt, getConfI ilpt ]
                     (Out1 A)

alpass :: Conf ARate -> Conf KRate -> Conf IRate -> Element ARate
alpass asig kvrt ilpt = 
    mkElement "alpass" [ getConfA asig, getConfK kvrt, getConfI ilpt ]
                       (Out1 A)

nestedap :: Conf ARate -> Conf IRate -> Conf IRate -> Conf IRate -> Conf IRate
         -> Element ARate
nestedap asig imode imaxdel idel1 igain1 = 
    mkElement "nestedap" [ getConfA asig, getConfI imode, getConfI imaxdel
                         , getConfI idel1, getConfI igain1 ]
                         (Out1 A)

--------------------------------------------------------------------------------
-- Panning and spatialization

-- Note - name /priority/ is given to the 4 channel versions.

-- | 4 channel pan.
--
pan :: Conf ARate -> Conf KRate -> Conf KRate -> Conf IRate -> Element ARate
pan asig kx ky ifn = 
    mkElement "pan" [ getConfA asig, getConfK kx, getConfK ky, getConfI ifn ]
                    (OutN A 4)

pan2 :: Conf ARate -> Conf KRate -> Conf KRate -> Conf IRate -> Element ARate
pan2 asig kx ky ifn = 
    mkElement "pan2" [ getConfA asig, getConfK kx, getConfK ky, getConfI ifn ]
                     (Out2 A)

-- | 4 channel version of locsig.
--
locsig :: Conf ARate -> Conf KRate -> Conf KRate -> Conf KRate 
       -> Element ARate
locsig asig kdegree kdistance kreverbsend = 
    mkElement "locsig" [ getConfA asig, getConfK kdegree
                       , getConfK kdistance, getConfK kreverbsend ]
                       (OutN A 4)


-- | This is the 2 channel version of 'locsig'
--
locsig2 :: Conf ARate -> Conf KRate -> Conf KRate -> Conf KRate 
        -> Element ARate
locsig2 asig kdegree kdistance kreverbsend = 
    mkElement "locsig" [ getConfA asig, getConfK kdegree
                       , getConfK kdistance, getConfK kreverbsend ]
                       (Out2 A)

