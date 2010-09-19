{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Text.LRSymbol
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Named literals from Symbol font, drawn with the LRText monad.
-- 
-- Note - currently the techinique used here generates adequate
-- PostScript, but very ineficient SVG.
-- 
-- Also uUpsilon is not mapped to the correct character...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Text.LRSymbol
  ( 


  -- * Upper case letters and upper case Greek letters
    uAlpha
  , uBeta
  , uChi
  , uDelta
  , uEpsilon
  , uEta
  , uEuro
  , uGamma
  , uIfraktur
  , uIota
  , uKappa
  , uLambda
  , uMu
  , uNu
  , uOmega
  , uOmicron
  , uPhi
  , uPi
  , uPsi
  , uRfraktur
  , uRho
  , uSigma
  , uTau
  , uTheta
  , uUpsilon
  , uUpsilon1
  , uXi
  , uZeta
  


  -- * Lower case Greek letters

  , alpha
  , beta
  , gamma
  , delta
  , epsilon
  , zeta
  , eta
  , theta
  , iota
  , kappa
  , lambda
  , mu
  , nu
  , xi
  , pi
  , rho
  , sigma
  , tau
  , upsilon
  , phi
  , chi
  , psi
  , omega

  -- * Miscellaneous chars
  , aleph
  , ampersand
  , angle
  , angleleft
  , angleright
  , approxequal
  , arrowboth
  , arrowdblboth
  , arrowdbldown
  , arrowdblleft
  , arrowdblright
  , arrowdblup
  , arrowdown
  , arrowleft
  , arrowright
  , arrowup
  , asteriskmath
  , bar
  , braceleft
  , braceright
  , bracketleft
  , bracketright

  , otimes
  , oplus

  ) where


import Wumpus.Basic.Text.LRText


import Prelude hiding ( pi )

-- Note - due to a optimization in Wumpus-Core, the PostScript
-- generated by the techinique here - writing a 
-- (single-character) label one at a time - isn\'t too bad as 
-- Wumpus-Core only issues a findfont command when the font 
-- changes. 
-- 
-- However for SVG the result is very poor - one text element 
-- including an orientation changing matrix transforming for 
-- each character.
-- 
-- Wumpus-Core\'s SVG rendering will work at some point.
-- 


--------------------------------------------------------------------------------
-- upper case

-- | Note - prints as \'A\'.
--
uAlpha          :: Num u => TextM u ()
uAlpha          = symb 'A'

-- | Note - prints as \'B\'.
--
uBeta           :: Num u => TextM u ()
uBeta           = symb 'B'

-- | Note - prints as \'X\'.
--
uChi            :: Num u => TextM u ()
uChi            = symb 'C'

uDelta          :: Num u => TextM u ()
uDelta          = symb 'D'

-- | Note - prints as \'E\'.
--
uEpsilon        :: Num u => TextM u ()
uEpsilon        = symb 'E'

-- | Note - prints as \'H\'.
--
uEta            :: Num u => TextM u ()
uEta            = symb 'H'


-- | Note - does not appear to print in Chrome.
--
uEuro           :: Num u => TextM u ()
uEuro           = symbEscInt 0o240

uGamma          :: Num u => TextM u ()
uGamma          = symb 'G'

uIfraktur       :: Num u => TextM u ()
uIfraktur       = symbEscInt 0o301

-- | Note - prints as \'I\'.
--
uIota           :: Num u => TextM u ()
uIota           = symb 'I'

-- | Note - prints as \'K\'.
--
uKappa          :: Num u => TextM u ()
uKappa          = symb 'K'

uLambda         :: Num u => TextM u ()
uLambda         = symb 'L'

-- | Note - prints as \'M\'.
--
uMu             :: Num u => TextM u ()
uMu             = symb 'M'

-- | Note - prints as \'N\'.
--
uNu             :: Num u => TextM u ()
uNu             = symb 'N'

uOmega          :: Num u => TextM u ()
uOmega          = symbEscInt 0o127

uOmicron        :: Num u => TextM u ()
uOmicron        = symbEscInt 0o117

uPhi            :: Num u => TextM u ()
uPhi            = symbEscInt 0o106

uPi             :: Num u => TextM u ()
uPi             = symb 'P'

uPsi            :: Num u => TextM u ()
uPsi            = symbEscInt 0o131

uRfraktur       :: Num u => TextM u ()
uRfraktur       = symbEscInt 0o302

uRho            :: Num u => TextM u ()
uRho            = symbEscInt 0o122

uSigma          :: Num u => TextM u ()
uSigma          = symb 'S'

uTau            :: Num u => TextM u ()
uTau            = symb 'T'


uTheta          :: Num u => TextM u ()
uTheta          = symb 'Q'

-- | Note - prints as \'Y\'.
--
uUpsilon        :: Num u => TextM u ()
uUpsilon        = symbEscInt 0o125

-- | Note - this is the /pretty/ Upsilon.
--
uUpsilon1       :: Num u => TextM u ()
uUpsilon1       = symbEscInt 0o241

uXi             :: Num u => TextM u ()
uXi             = symb 'X'

uZeta           :: Num u => TextM u ()
uZeta           = symb 'Z'


--------------------------------------------------------------------------------
-- lower case Greek letters


alpha           :: Num u => TextM u ()
alpha           = symb 'a'

beta            :: Num u => TextM u ()
beta            = symb 'b'

gamma           :: Num u => TextM u ()
gamma           = symb 'g'

delta           :: Num u => TextM u ()
delta           = symb 'd'

epsilon         :: Num u => TextM u ()
epsilon         = symb 'e'

zeta            :: Num u => TextM u ()
zeta            = symb 'z'

eta             :: Num u => TextM u ()
eta             = symb 'h'

theta           :: Num u => TextM u ()
theta           = symb 'q'

iota            :: Num u => TextM u ()
iota            = symb 'i'

kappa           :: Num u => TextM u ()
kappa           = symb 'k'

lambda          :: Num u => TextM u ()
lambda          = symb 'l'

mu              :: Num u => TextM u ()
mu              = symb 'm'

nu              :: Num u => TextM u ()
nu              = symb 'n'

xi              :: Num u => TextM u ()
xi              = symb 'x'

pi              :: Num u => TextM u ()
pi              = symb 'p'

rho             :: Num u => TextM u ()
rho             = symb 'r'

sigma           :: Num u => TextM u ()
sigma           = symb 's'

tau             :: Num u => TextM u ()
tau             = symb 't'

upsilon         :: Num u => TextM u ()
upsilon         = symb 'u'

phi             :: Num u => TextM u ()
phi             = symb 'j'

chi             :: Num u => TextM u ()
chi             = symb 'c'

psi             :: Num u => TextM u ()
psi             = symb 'y'

omega           :: Num u => TextM u ()
omega           = symb 'w'



--------------------------------------------------------------------------------
-- Miscellaneous chars

aleph           :: Num u => TextM u ()
aleph           = symbEscInt 0o300

ampersand       :: Num u => TextM u ()
ampersand       = symbEscInt 0o046

angle           :: Num u => TextM u ()
angle           = symbEscInt 0o320

angleleft       :: Num u => TextM u ()
angleleft       = symbEscInt 0o341

angleright      :: Num u => TextM u ()
angleright      = symbEscInt 0o361

approxequal     :: Num u => TextM u ()
approxequal     = symbEscInt 0o273

arrowboth       :: Num u => TextM u ()
arrowboth       = symbEscInt 0o253

arrowdblboth    :: Num u => TextM u ()
arrowdblboth    = symbEscInt 0o333

arrowdbldown    :: Num u => TextM u ()
arrowdbldown    = symbEscInt 0o337

arrowdblleft    :: Num u => TextM u ()
arrowdblleft    = symbEscInt 0o334

arrowdblright   :: Num u => TextM u ()
arrowdblright   = symbEscInt 0o336

arrowdblup      :: Num u => TextM u ()
arrowdblup      = symbEscInt 0o335

arrowdown       :: Num u => TextM u ()
arrowdown       = symbEscInt 0o257

arrowleft       :: Num u => TextM u ()
arrowleft       = symbEscInt 0o254

arrowright      :: Num u => TextM u ()
arrowright      = symbEscInt 0o256

arrowup         :: Num u => TextM u ()
arrowup         = symbEscInt 0o255

asteriskmath    :: Num u => TextM u ()
asteriskmath    = symbEscInt 0o052

bar             :: Num u => TextM u ()
bar             = symbEscInt 0o174

braceleft       :: Num u => TextM u ()
braceleft       = symbEscInt 0o173

braceright      :: Num u => TextM u ()
braceright      = symbEscInt 0o175

bracketleft     :: Num u => TextM u ()
bracketleft     = symbEscInt 0o133

bracketright    :: Num u => TextM u ()
bracketright    = symbEscInt 0o135


otimes      :: Num u => TextM u ()
otimes      = symbi 0xc4

oplus       :: Num u => TextM u ()
oplus       = symbi 0xc5
