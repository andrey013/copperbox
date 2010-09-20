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
  , bullet
  , carriagereturn
  , circlemultiply
  , circleplus
  , club 
  , colon
  , comma
  , congruent
  , copyrightsans
  , copyrightserif
  , degree
  , diamond
  , divide
  , dotmath
  , eight
  , element
  , ellipsis
  , emptyset
  , equal
  , equivalence
  , exclam
  , existential
  , five
  , florin
  , four
  , fraction
  , gradient
  , greater
  , greaterequal
  , heart
  , infinity
  , integral
  , intersection
  , less
  , lessequal
  , logicaland
  , logicalnot
  , logicalor
  , lozenge
  , minus
  , minute
  , multiply
  , nine
  , notelement
  , notequal
  , notsubset
  , numbersign
  , omega1
  , omicron
  , one
  , parenleft
  , parenright
  , partialdiff
  , percent
  , period
  , perpendicular
  , phi1
  , plus
  , plusminus
  , product
  , propersubset
  , propersuperset
  , proportional
  , question
  , radical
  , radicalex
  , reflexsubset
  , reflexsuperset
  , registersans
  , registerserif
  , second
  , semicolon
  , seven
  , sigma1
  , similar
  , six
  , slash
  , space
  , spade
  , suchthat
  , summation
  , therefore
  , theta1
  , three
  , trademarksans
  , trademarkserif
  , two
  , underscore
  , union
  , universal
  , weierstrass
  , zero

  ) where


import Wumpus.Basic.Text.LRText


import Prelude hiding ( pi, product )



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

bullet          :: Num u => TextM u ()
bullet          = symbEscInt 0o267

carriagereturn  :: Num u => TextM u ()
carriagereturn  = symbEscInt 0o277


circlemultiply  :: Num u => TextM u ()
circlemultiply  = symbEscInt 0o304

circleplus      :: Num u => TextM u ()
circleplus      = symbEscInt 0o305

club            :: Num u => TextM u ()
club            = symbEscInt 0o247

colon           :: Num u => TextM u ()
colon           = symbEscInt 0o072

comma           :: Num u => TextM u ()
comma           = symbEscInt 0o054

congruent       :: Num u => TextM u ()
congruent             = symbEscInt 0o100

copyrightsans   :: Num u => TextM u ()
copyrightsans         = symbEscInt 0o343

copyrightserif  :: Num u => TextM u ()
copyrightserif  = symbEscInt 0o323

degree          :: Num u => TextM u ()
degree          = symbEscInt 0o260

diamond         :: Num u => TextM u ()
diamond         = symbEscInt 0o250

divide          :: Num u => TextM u ()
divide          = symbEscInt 0o270

dotmath         :: Num u => TextM u ()
dotmath         = symbEscInt 0o327

eight           :: Num u => TextM u ()
eight           = symbEscInt 0o070

element         :: Num u => TextM u ()
element         = symbEscInt 0o316

ellipsis        :: Num u => TextM u ()
ellipsis        = symbEscInt 0o274

emptyset        :: Num u => TextM u ()
emptyset        = symbEscInt 0o306

equal           :: Num u => TextM u ()
equal           = symbEscInt 0o075

equivalence     :: Num u => TextM u ()
equivalence     = symbEscInt 0o272

exclam          :: Num u => TextM u ()
exclam          = symbEscInt 0o041

existential     :: Num u => TextM u ()
existential     = symbEscInt 0o044

five            :: Num u => TextM u ()
five            = symbEscInt 0o065

florin          :: Num u => TextM u ()
florin          = symbEscInt 0o246

four            :: Num u => TextM u ()
four            = symbEscInt 0o064

fraction        :: Num u => TextM u ()
fraction        = symbEscInt 0o244

gradient        :: Num u => TextM u ()
gradient        = symbEscInt 0o321

greater         :: Num u => TextM u ()
greater         = symbEscInt 0o076

greaterequal    :: Num u => TextM u ()
greaterequal    = symbEscInt 0o263

heart           :: Num u => TextM u ()
heart           = symbEscInt 0o251

infinity        :: Num u => TextM u ()
infinity        = symbEscInt 0o245

integral        :: Num u => TextM u ()
integral        = symbEscInt 0o362

intersection    :: Num u => TextM u ()
intersection    = symbEscInt 0o307

less            :: Num u => TextM u ()
less            = symbEscInt 0o074

lessequal       :: Num u => TextM u ()
lessequal       = symbEscInt 0o243

logicaland      :: Num u => TextM u ()
logicaland      = symbEscInt 0o331

logicalnot      :: Num u => TextM u ()
logicalnot      = symbEscInt 0o330

logicalor       :: Num u => TextM u ()
logicalor       = symbEscInt 0o332

lozenge         :: Num u => TextM u ()
lozenge         = symbEscInt 0o340

minus           :: Num u => TextM u ()
minus           = symbEscInt 0o055

minute          :: Num u => TextM u ()
minute          = symbEscInt 0o242

multiply        :: Num u => TextM u ()
multiply        = symbEscInt 0o264

nine            :: Num u => TextM u ()
nine            = symbEscInt 0o071

notelement      :: Num u => TextM u ()
notelement      = symbEscInt 0o317

notequal        :: Num u => TextM u ()
notequal        = symbEscInt 0o271

notsubset       :: Num u => TextM u ()
notsubset       = symbEscInt 0o313

numbersign      :: Num u => TextM u ()
numbersign      = symbEscInt 0o043

omega1          :: Num u => TextM u ()
omega1          = symbEscInt 0o166

omicron         :: Num u => TextM u ()
omicron         = symbEscInt 0o157

one             :: Num u => TextM u ()
one             = symbEscInt 0o061

parenleft       :: Num u => TextM u ()
parenleft       = symbEscInt 0o050

parenright      :: Num u => TextM u ()
parenright      = symbEscInt 0o051

partialdiff     :: Num u => TextM u ()
partialdiff     = symbEscInt 0o266

percent         :: Num u => TextM u ()
percent         = symbEscInt 0o045

period          :: Num u => TextM u ()
period          = symbEscInt 0o056

perpendicular   :: Num u => TextM u ()
perpendicular   = symbEscInt 0o136

phi1            :: Num u => TextM u ()
phi1            = symbEscInt 0o152

plus            :: Num u => TextM u ()
plus            = symbEscInt 0o053

plusminus       :: Num u => TextM u ()
plusminus       = symbEscInt 0o261

product         :: Num u => TextM u ()
product         = symbEscInt 0o325

propersubset    :: Num u => TextM u ()
propersubset    = symbEscInt 0o314

propersuperset  :: Num u => TextM u ()
propersuperset  = symbEscInt 0o311

proportional    :: Num u => TextM u ()
proportional    = symbEscInt 0o265

question        :: Num u => TextM u ()
question        = symbEscInt 0o077

radical         :: Num u => TextM u ()
radical         = symbEscInt 0o326

radicalex       :: Num u => TextM u ()
radicalex       = symbEscInt 0o140

reflexsubset    :: Num u => TextM u ()
reflexsubset    = symbEscInt 0o315

reflexsuperset  :: Num u => TextM u ()
reflexsuperset  = symbEscInt 0o312

registersans    :: Num u => TextM u ()
registersans    = symbEscInt 0o342

registerserif   :: Num u => TextM u ()
registerserif   = symbEscInt 0o322

second          :: Num u => TextM u ()
second          = symbEscInt 0o262

semicolon       :: Num u => TextM u ()
semicolon       = symbEscInt 0o073

seven           :: Num u => TextM u ()
seven           = symbEscInt 0o067

sigma1          :: Num u => TextM u ()
sigma1          = symbEscInt 0o126

similar         :: Num u => TextM u ()
similar         = symbEscInt 0o176

six             :: Num u => TextM u ()
six             = symbEscInt 0o066

slash           :: Num u => TextM u ()
slash           = symbEscInt 0o057

space           :: Num u => TextM u ()
space           = symbEscInt 0o040

spade           :: Num u => TextM u ()
spade           = symbEscInt 0o252

suchthat        :: Num u => TextM u ()
suchthat        = symbEscInt 0o047

summation       :: Num u => TextM u ()
summation       = symbEscInt 0o345

therefore       :: Num u => TextM u ()
therefore       = symbEscInt 0o134

theta1          :: Num u => TextM u ()
theta1          = symbEscInt 0o112

three           :: Num u => TextM u ()
three           = symbEscInt 0o063

trademarksans   :: Num u => TextM u ()
trademarksans   = symbEscInt 0o344

trademarkserif  :: Num u => TextM u ()
trademarkserif  = symbEscInt 0o324

two             :: Num u => TextM u ()
two             = symbEscInt 0o062

underscore      :: Num u => TextM u ()
underscore      = symbEscInt 0o137

union           :: Num u => TextM u ()
union           = symbEscInt 0o310

universal       :: Num u => TextM u ()
universal       = symbEscInt 0o042

weierstrass     :: Num u => TextM u ()
weierstrass     = symbEscInt 0o303

zero            :: Num u => TextM u ()
zero            = symbEscInt 0o060


