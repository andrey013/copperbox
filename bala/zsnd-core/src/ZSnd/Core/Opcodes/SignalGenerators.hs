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
  , oscili
  , oscil3
  , poscil
  , poscil3
  , lfo

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

  -- * Sample playback
  , loscil
  , biloscil

  ) where


import ZSnd.Core.CsoundInst

--------------------------------------------------------------------------------
-- Linear and Exponential generators

class CLinear rate where
  line      :: Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
  expon     :: Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
  linseg    :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> InstBuilder (Expr rate)

  linsegr   :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  expseg    :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> InstBuilder (Expr rate)

  expsegr   :: Expr IR -> Expr IR -> Expr IR -> [(Expr IR, Expr IR)]
                       -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)



instance CLinear KR where
  line ia idur ib = opcode "line" [ getExpr ia, getExpr idur, getExpr ib ]

  expon ia idur ib = opcode "expon" [ getExpr ia, getExpr idur, getExpr ib ]

  linseg ia idur ib xs = 
    opcode "linseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  linsegr ia idur ib xs irel iz = 
    opcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]

  expseg ia idur ib xs = 
    opcode "expseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  expsegr ia idur ib xs irel iz = 
    opcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]


instance CLinear AR where
  line ia idur ib = opcode "line" [ getExpr ia, getExpr idur, getExpr ib ]

  expon ia idur ib = opcode "expon" [ getExpr ia, getExpr idur, getExpr ib ]

  linseg ia idur ib xs = 
    opcode "linseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  linsegr ia idur ib xs irel iz = 
    opcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]

  expseg ia idur ib xs = 
    opcode "expseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  expsegr ia idur ib xs irel iz = 
    opcode "expsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]



expsega   :: Expr IR -> Expr IR -> Expr IR 
                        -> [(Expr IR, Expr IR)]
                        -> InstBuilder (Expr AR)
expsega ia idur ib xs = 
    opcode "expsega" (getExpr ia : getExpr idur : getExpr ib : rest)
  where
    rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs


class CAdsr rate where
  adsr      :: Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  adsr_     :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  madsr     :: Expr IR -> Expr IR -> Expr IR -> Expr IR
                       -> InstBuilder (Expr rate)

  madsr_    :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  xadsr     :: Expr IR -> Expr IR -> Expr IR -> Expr IR
                       -> InstBuilder (Expr rate)

  xadsr_    :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  mxadsr    :: Expr IR -> Expr IR -> Expr IR -> Expr IR
                       -> InstBuilder (Expr rate)

  mxadsr_   :: Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

instance CAdsr KR where
  adsr ia idec isl ir = 
    opcode "adsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  adsr_ ia idec isl ir idel = 
    opcode "adsr" [ getExpr ia, getExpr idec, getExpr isl
                  , getExpr ir, getExpr idel ]

  madsr ia idec isl ir = 
    opcode "madsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  madsr_ ia idec isl ir idel = 
    opcode "madsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

  xadsr ia idec isl ir = 
    opcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  xadsr_ ia idec isl ir idel = 
    opcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

  mxadsr ia idec isl ir = 
    opcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  mxadsr_ ia idec isl ir idel = 
    opcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl
                    , getExpr ir, getExpr idel ]

instance CAdsr AR where
  adsr ia idec isl ir = 
    opcode "adsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  adsr_ ia idec isl ir idel = 
    opcode "adsr" [ getExpr ia, getExpr idec, getExpr isl
                  , getExpr ir, getExpr idel ]

  madsr ia idec isl ir = 
    opcode "madsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  madsr_ ia idec isl ir idel = 
    opcode "madsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

  xadsr ia idec isl ir = 
    opcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  xadsr_ ia idec isl ir idel = 
    opcode "xadsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

  mxadsr ia idec isl ir = 
    opcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  mxadsr_ ia idec isl ir idel = 
    opcode "mxadsr" [ getExpr ia, getExpr idec, getExpr isl
                    , getExpr ir, getExpr idel ]

--------------------------------------------------------------------------------
-- Table access

table :: Opcode rate 
      => Expr rate -> Expr IR -> InstBuilder (Expr rate)
table ndx ifn = opcode "table" [ getExpr ndx, getExpr ifn ]

table_ :: Opcode rate
       => Expr rate -> Expr IR -> Expr rate -> Expr IR -> Expr IR    
       -> InstBuilder (Expr rate)
table_ ndx ifn ixmode ixoff ixwrap = 
    opcode "table" [ getExpr ndx, getExpr ifn, getExpr ixmode
                   , getExpr ixoff, getExpr ixwrap ]


tablei :: Opcode rate
       => Expr rate -> Expr IR -> InstBuilder (Expr rate)
tablei ndx ifn = opcode "tablei" [ getExpr ndx, getExpr ifn ]

tablei_ :: Opcode rate
        => Expr rate -> Expr IR -> Expr rate -> Expr IR -> Expr IR
       -> InstBuilder (Expr rate)
tablei_ ndx ifn ixmode ixoff ixwrap = 
    opcode "tablei" [ getExpr ndx, getExpr ifn, getExpr ixmode
                    , getExpr ixoff, getExpr ixwrap ]



table3 :: Opcode rate
       => Expr rate -> Expr IR -> InstBuilder (Expr rate)
table3 ndx ifn = opcode "table" [ getExpr ndx, getExpr ifn ]

table3_ :: Opcode rate
        => Expr rate -> Expr IR -> Expr rate -> Expr IR -> Expr IR
        -> InstBuilder (Expr rate)
table3_ ndx ifn ixmode ixoff ixwrap = 
    opcode "table3" [ getExpr ndx, getExpr ifn, getExpr ixmode
                    , getExpr ixoff, getExpr ixwrap ]


oscil1 :: Expr IR -> Expr KR -> Expr IR -> Expr IR -> InstBuilder (Expr KR)
oscil1 idel kamp idur ifn = 
    opcode "oscil1" [ getExpr idel, getExpr kamp, getExpr idur, getExpr ifn ]


oscil1i :: Expr IR -> Expr KR -> Expr IR -> Expr IR -> InstBuilder (Expr KR)
oscil1i idel kamp idur ifn = 
    opcode "oscil1i" [ getExpr idel, getExpr kamp, getExpr idur, getExpr ifn ]

osciln :: Expr KR -> Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr KR)
osciln kamp ifrq ifn itimes = 
    opcode "osciln" [ getExpr kamp, getExpr ifrq, getExpr ifn, getExpr itimes ]

--------------------------------------------------------------------------------
-- Phasors

class CPhasor rate where
  phasor      :: Expr rate -> InstBuilder (Expr rate)
  phasor_     :: Expr rate -> Expr IR -> InstBuilder (Expr rate)

  phasorbnk   :: Expr rate -> Expr KR -> Expr IR -> InstBuilder (Expr rate)
  phasorbnk_  :: Expr rate -> Expr KR -> Expr IR -> Expr IR 
                           -> InstBuilder (Expr rate)
 
instance CPhasor KR where
  phasor cps = opcode "phasor" [getExpr cps]
  phasor_ cps iphs = opcode "phasor" [getExpr cps, getExpr iphs]
   
  phasorbnk cps kindx icnt = 
    opcode "phasorbnk" [ getExpr cps, getExpr kindx, getExpr icnt ]

  phasorbnk_ cps kindx icnt iphs = 
    opcode "phasorbnk" [ getExpr cps, getExpr kindx
                       , getExpr icnt, getExpr iphs ]

 
instance CPhasor AR where
  phasor cps = opcode "phasor" [getExpr cps]
  phasor_ cps iphs = opcode "phasor" [getExpr cps, getExpr iphs]
   
  phasorbnk cps kindx icnt = 
    opcode "phasorbnk" [ getExpr cps, getExpr kindx, getExpr icnt ]

  phasorbnk_ cps kindx icnt iphs = 
    opcode "phasorbnk" [ getExpr cps, getExpr kindx
                       , getExpr icnt, getExpr iphs ]


--------------------------------------------------------------------------------
-- Basic oscillators

class COscil rate where
  oscil     :: Expr rate -> Expr rate -> Expr IR -> InstBuilder (Expr rate)

  oscil_    :: Expr rate -> Expr rate -> Expr IR -> Expr IR 
                         -> InstBuilder (Expr rate)

  oscili    :: Expr rate -> Expr rate -> Expr IR -> InstBuilder (Expr rate)
  oscili_   :: Expr rate -> Expr rate -> Expr IR -> Expr IR 
                         -> InstBuilder (Expr rate)

  oscil3    :: Expr rate -> Expr rate -> Expr IR -> InstBuilder (Expr rate)
  oscil3_   :: Expr rate -> Expr rate -> Expr IR -> Expr IR 
                         -> InstBuilder (Expr rate)

  poscil    :: Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr rate)
  poscil_   :: Expr KR -> Expr KR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  poscil3   :: Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr rate)
  poscil3_  :: Expr KR -> Expr KR -> Expr IR -> Expr IR 
                       -> InstBuilder (Expr rate)

  lfo       :: Expr KR -> Expr KR -> InstBuilder (Expr rate)
  lfo_      :: Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr rate)

instance COscil KR where
  oscil amp cps ifn  = 
    opcode "oscil" [getExpr amp, getExpr cps, getExpr ifn]

  oscil_ amp cps ifn iphs = 
    opcode "oscil" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscili amp cps ifn = 
    opcode "oscili" [getExpr amp, getExpr cps, getExpr ifn]

  oscili_ amp cps ifn iphs = 
    opcode "oscili" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscil3 amp cps ifn = 
    opcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn]

  oscil3_ amp cps ifn iphs = 
    opcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  poscil amp cps ift = 
    opcode "poscil" [getExpr amp, getExpr cps, getExpr ift]

  poscil_ amp cps ift iphs = 
    opcode "poscil" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  poscil3 amp cps ift = 
    opcode "poscil3" [getExpr amp, getExpr cps, getExpr ift]

  poscil3_ amp cps ift iphs = 
    opcode "poscil3" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  lfo amp cps = 
    opcode "lfo" [getExpr amp, getExpr cps]

  lfo_ amp cps itype = 
    opcode "lfo" [getExpr amp, getExpr cps, getExpr itype]


instance COscil AR where
  oscil amp cps ifn = 
    opcode "oscil" [getExpr amp, getExpr cps, getExpr ifn]

  oscil_ amp cps ifn iphs = 
    opcode "oscil" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscili amp cps ifn = 
    opcode "oscili" [getExpr amp, getExpr cps, getExpr ifn]

  oscili_ amp cps ifn iphs = 
    opcode "oscili" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  oscil3 amp cps ifn = 
    opcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn]

  oscil3_ amp cps ifn iphs = 
    opcode "oscil3" [getExpr amp, getExpr cps, getExpr ifn, getExpr iphs]

  poscil amp cps ift = 
    opcode "poscil" [getExpr amp, getExpr cps, getExpr ift]

  poscil_ amp cps ift iphs = 
    opcode "poscil" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  poscil3 amp cps ift = 
    opcode "poscil3" [getExpr amp, getExpr cps, getExpr ift]

  poscil3_ amp cps ift iphs = 
    opcode "poscil3" [getExpr amp, getExpr cps, getExpr ift, getExpr iphs]

  lfo amp cps = 
    opcode "lfo" [getExpr amp, getExpr cps]

  lfo_ amp cps itype = 
    opcode "lfo" [getExpr amp, getExpr cps, getExpr itype]


--------------------------------------------------------------------------------
-- Dynamic spectrum oscillators

-- TODO - what does the x prefix convention mean for Csound?
-- Above we have interpreted it as audio rate, but is this 
-- correct?

buzz :: Expr a -> Expr a -> Expr KR -> Expr IR -> InstBuilder (Expr AR)
buzz xamp xcps knh ifn = 
    opcode "buzz" [getExpr xamp, getExpr xcps, getExpr knh, getExpr ifn]

buzz_ :: Expr a -> Expr a -> Expr KR -> Expr IR -> Expr IR 
      -> InstBuilder (Expr AR)
buzz_ xamp xcps knh ifn iphs = 
    opcode "buzz" [ getExpr xamp, getExpr xcps, getExpr knh
                  , getExpr ifn, getExpr iphs ]


gbuzz :: Expr a -> Expr a -> Expr KR -> Expr IR -> Expr KR -> Expr KR 
      -> InstBuilder (Expr AR)
gbuzz xamp xcps knh klh kr ifn = 
    opcode "gbuzz" [ getExpr xamp, getExpr xcps, getExpr knh
                   , getExpr klh,  getExpr kr,   getExpr ifn ]

gbuzz_ :: Expr a -> Expr a -> Expr KR -> Expr IR 
       -> Expr KR -> Expr KR -> Expr IR
       -> InstBuilder (Expr AR)
gbuzz_ xamp xcps knh klh kr ifn iphs = 
    opcode "gbuzz" [ getExpr xamp, getExpr xcps, getExpr knh
                   , getExpr klh,  getExpr kr,   getExpr ifn, getExpr iphs ]


vco :: Expr KR -> Expr KR -> Expr IR -> Expr KR -> Expr IR -> Expr IR 
    -> InstBuilder (Expr AR)
vco kamp kfqc iwave kpw ifn imaxd = 
    opcode "gbuzz" [ getExpr kamp, getExpr kfqc, getExpr iwave
                   , getExpr kpw,  getExpr ifn,  getExpr imaxd ]

--------------------------------------------------------------------------------
-- Additive synthesis / resynthesis

adsyn :: Expr KR -> Expr KR -> Expr KR -> Expr IR -> InstBuilder (Expr AR)
adsyn kamod kfmod ksmod ifilcod = 
    opcode "adsyn" [ getExpr kamod, getExpr kfmod, getExpr ksmod
                    , getExpr ifilcod ]


adsynt :: Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
       -> InstBuilder (Expr AR)
adsynt kamp kcps iwfn ifreqfn iampfn icnt = 
    opcode "adsynt" [ getExpr kamp, getExpr kcps, getExpr iwfn
                     , getExpr ifreqfn, getExpr iampfn, getExpr icnt ]

adsynt_ :: Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> Expr IR
        -> InstBuilder (Expr AR)
adsynt_ kamp kcps iwfn ifreqfn iampfn icnt iphs = 
    opcode "adsynt" [ getExpr kamp, getExpr kcps, getExpr iwfn
                     , getExpr ifreqfn, getExpr iampfn, getExpr icnt
                     , getExpr iphs ]


hsboscil :: Expr KR -> Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR
         -> InstBuilder (Expr AR)
hsboscil kamp ktone kbrite ibasfreq iwfn ioctfn = 
    opcode "hsboscil" [ getExpr kamp, getExpr ktone, getExpr kbrite
                      , getExpr ibasfreq, getExpr iwfn, getExpr ioctfn ]

hsboscil_ :: Expr KR -> Expr KR -> Expr KR -> Expr IR -> Expr IR -> Expr IR
          -> Expr IR
          -> InstBuilder (Expr AR)
hsboscil_ kamp ktone kbrite ibasfreq iwfn ioctfn iphs = 
    opcode "hsboscil" [ getExpr kamp, getExpr ktone, getExpr kbrite
                      , getExpr ibasfreq, getExpr iwfn, getExpr ioctfn
                      , getExpr iphs ]

--------------------------------------------------------------------------------
-- FM Synthesis

foscil :: Expr a -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr IR
       -> InstBuilder (Expr AR)
foscil xamp kcps kcar kmod kndx ifn = 
    opcode "foscil" [ getExpr xamp, getExpr kcps, getExpr kcar
                    , getExpr kmod, getExpr kndx, getExpr ifn ]


foscil_ :: Expr a -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr IR 
        -> Expr IR
        -> InstBuilder (Expr AR)
foscil_ xamp kcps kcar kmod kndx ifn iphs = 
    opcode "foscil" [ getExpr xamp, getExpr kcps, getExpr kcar
                    , getExpr kmod, getExpr kndx, getExpr ifn
                    , getExpr iphs ]

foscili :: Expr a -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr IR
        -> InstBuilder (Expr AR)
foscili xamp kcps kcar kmod kndx ifn = 
    opcode "foscili" [ getExpr xamp, getExpr kcps, getExpr kcar
                     , getExpr kmod, getExpr kndx, getExpr ifn ]



foscili_ :: Expr a -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr IR 
         -> Expr IR
         -> InstBuilder (Expr AR)
foscili_ xamp kcps kcar kmod kndx ifn iphs = 
    opcode "foscil" [ getExpr xamp, getExpr kcps, getExpr kcar
                    , getExpr kmod, getExpr kndx, getExpr ifn
                    , getExpr iphs ]


fmvoice :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
        -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> InstBuilder (Expr AR)
fmvoice kamp kfreq kvowel ktilt kvibamt kvibrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmvoice" [ getExpr kamp, getExpr kfreq, getExpr kvowel
                     , getExpr ktilt, getExpr kvibamt, getExpr kvibrate
                     , getExpr ifn1, getExpr ifn2, getExpr ifn3
                     , getExpr ifn4, getExpr ivibfn ]

fmbell :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
       -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
       -> InstBuilder (Expr AR)
fmbell kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmbell" [ getExpr kamp, getExpr kfreq, getExpr kc1
                     , getExpr kc2, getExpr kvdepth, getExpr kvrate
                     , getExpr ifn1, getExpr ifn2, getExpr ifn3
                     , getExpr ifn4, getExpr ivibfn ]

fmrhode :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
        -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> InstBuilder (Expr AR)
fmrhode kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmrhode" [ getExpr kamp, getExpr kfreq, getExpr kc1
                     , getExpr kc2, getExpr kvdepth, getExpr kvrate
                     , getExpr ifn1, getExpr ifn2, getExpr ifn3
                     , getExpr ifn4, getExpr ivibfn ]

fmwurlie :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
        -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> InstBuilder (Expr AR)
fmwurlie kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmwurlie" [ getExpr kamp, getExpr kfreq, getExpr kc1
                      , getExpr kc2, getExpr kvdepth, getExpr kvrate
                      , getExpr ifn1, getExpr ifn2, getExpr ifn3
                      , getExpr ifn4, getExpr ivibfn ]

fmmetal :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
        -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> InstBuilder (Expr AR)
fmmetal kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmmetal" [ getExpr kamp, getExpr kfreq, getExpr kc1
                     , getExpr kc2, getExpr kvdepth, getExpr kvrate
                     , getExpr ifn1, getExpr ifn2, getExpr ifn3
                     , getExpr ifn4, getExpr ivibfn ]


fmb3 :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
        -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
        -> InstBuilder (Expr AR)
fmb3 kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmb3" [ getExpr kamp, getExpr kfreq, getExpr kc1
                  , getExpr kc2, getExpr kvdepth, getExpr kvrate
                  , getExpr ifn1, getExpr ifn2, getExpr ifn3
                  , getExpr ifn4, getExpr ivibfn ]


fmpercfl :: Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR -> Expr KR
         -> Expr IR -> Expr IR -> Expr IR -> Expr IR -> Expr IR
         -> InstBuilder (Expr AR)
fmpercfl kamp kfreq kc1 kc2 kvdepth kvrate ifn1 ifn2 ifn3 ifn4 ivibfn = 
    opcode "fmpercfl" [ getExpr kamp, getExpr kfreq, getExpr kc1
                      , getExpr kc2, getExpr kvdepth, getExpr kvrate
                      , getExpr ifn1, getExpr ifn2, getExpr ifn3
                      , getExpr ifn4, getExpr ivibfn ]

--------------------------------------------------------------------------------
-- Sample playback

-- Note - it seems idiomatic to want a stereo version will only 
-- the mandatory args...



loscil :: Expr a -> Expr KR -> Expr IR -> Expr IR 
       -> InstBuilder (Expr AR)
loscil xamp kcps ifn ibase = 
    opcode "loscil" [ getExpr xamp, getExpr kcps, getExpr ifn, getExpr ibase]


biloscil :: Expr a -> Expr KR -> Expr IR -> Expr IR 
       -> InstBuilder (Expr AR, Expr AR)
biloscil xamp kcps ifn ibase = 
    biopcode "loscil" [ getExpr xamp, getExpr kcps, getExpr ifn, getExpr ibase]
