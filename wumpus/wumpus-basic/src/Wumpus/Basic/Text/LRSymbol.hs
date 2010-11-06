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
-- \*\* WARNING \*\* this module is considered obsolete.
-- 
-- LRText no longer seems a satisfactory way to build text.
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
uAlpha          :: Num u => LRText u ()
uAlpha          = symb 'A'

-- | Note - prints as \'B\'.
--
uBeta           :: Num u => LRText u ()
uBeta           = symb 'B'

-- | Note - prints as \'X\'.
--
uChi            :: Num u => LRText u ()
uChi            = symb 'C'

uDelta          :: Num u => LRText u ()
uDelta          = symb 'D'

-- | Note - prints as \'E\'.
--
uEpsilon        :: Num u => LRText u ()
uEpsilon        = symb 'E'

-- | Note - prints as \'H\'.
--
uEta            :: Num u => LRText u ()
uEta            = symb 'H'


-- | Note - does not appear to print in Chrome.
--
uEuro           :: Num u => LRText u ()
uEuro           = symbEscInt 0o240

uGamma          :: Num u => LRText u ()
uGamma          = symb 'G'

uIfraktur       :: Num u => LRText u ()
uIfraktur       = symbEscInt 0o301

-- | Note - prints as \'I\'.
--
uIota           :: Num u => LRText u ()
uIota           = symb 'I'

-- | Note - prints as \'K\'.
--
uKappa          :: Num u => LRText u ()
uKappa          = symb 'K'

uLambda         :: Num u => LRText u ()
uLambda         = symb 'L'

-- | Note - prints as \'M\'.
--
uMu             :: Num u => LRText u ()
uMu             = symb 'M'

-- | Note - prints as \'N\'.
--
uNu             :: Num u => LRText u ()
uNu             = symb 'N'

uOmega          :: Num u => LRText u ()
uOmega          = symbEscInt 0o127

uOmicron        :: Num u => LRText u ()
uOmicron        = symbEscInt 0o117

uPhi            :: Num u => LRText u ()
uPhi            = symbEscInt 0o106

uPi             :: Num u => LRText u ()
uPi             = symb 'P'

uPsi            :: Num u => LRText u ()
uPsi            = symbEscInt 0o131

uRfraktur       :: Num u => LRText u ()
uRfraktur       = symbEscInt 0o302

uRho            :: Num u => LRText u ()
uRho            = symbEscInt 0o122

uSigma          :: Num u => LRText u ()
uSigma          = symb 'S'

uTau            :: Num u => LRText u ()
uTau            = symb 'T'


uTheta          :: Num u => LRText u ()
uTheta          = symb 'Q'

-- | Note - prints as \'Y\'.
--
uUpsilon        :: Num u => LRText u ()
uUpsilon        = symbEscInt 0o125

-- | Note - this is the /pretty/ Upsilon.
--
uUpsilon1       :: Num u => LRText u ()
uUpsilon1       = symbEscInt 0o241

uXi             :: Num u => LRText u ()
uXi             = symb 'X'

uZeta           :: Num u => LRText u ()
uZeta           = symb 'Z'


--------------------------------------------------------------------------------
-- lower case Greek letters


alpha           :: Num u => LRText u ()
alpha           = symb 'a'

beta            :: Num u => LRText u ()
beta            = symb 'b'

gamma           :: Num u => LRText u ()
gamma           = symb 'g'

delta           :: Num u => LRText u ()
delta           = symb 'd'

epsilon         :: Num u => LRText u ()
epsilon         = symb 'e'

zeta            :: Num u => LRText u ()
zeta            = symb 'z'

eta             :: Num u => LRText u ()
eta             = symb 'h'

theta           :: Num u => LRText u ()
theta           = symb 'q'

iota            :: Num u => LRText u ()
iota            = symb 'i'

kappa           :: Num u => LRText u ()
kappa           = symb 'k'

lambda          :: Num u => LRText u ()
lambda          = symb 'l'

mu              :: Num u => LRText u ()
mu              = symb 'm'

nu              :: Num u => LRText u ()
nu              = symb 'n'

xi              :: Num u => LRText u ()
xi              = symb 'x'

pi              :: Num u => LRText u ()
pi              = symb 'p'

rho             :: Num u => LRText u ()
rho             = symb 'r'

sigma           :: Num u => LRText u ()
sigma           = symb 's'

tau             :: Num u => LRText u ()
tau             = symb 't'

upsilon         :: Num u => LRText u ()
upsilon         = symb 'u'

phi             :: Num u => LRText u ()
phi             = symb 'j'

chi             :: Num u => LRText u ()
chi             = symb 'c'

psi             :: Num u => LRText u ()
psi             = symb 'y'

omega           :: Num u => LRText u ()
omega           = symb 'w'



--------------------------------------------------------------------------------
-- Miscellaneous chars

aleph           :: Num u => LRText u ()
aleph           = symbEscInt 0o300

ampersand       :: Num u => LRText u ()
ampersand       = symbEscInt 0o046

angle           :: Num u => LRText u ()
angle           = symbEscInt 0o320

angleleft       :: Num u => LRText u ()
angleleft       = symbEscInt 0o341

angleright      :: Num u => LRText u ()
angleright      = symbEscInt 0o361

approxequal     :: Num u => LRText u ()
approxequal     = symbEscInt 0o273

arrowboth       :: Num u => LRText u ()
arrowboth       = symbEscInt 0o253

arrowdblboth    :: Num u => LRText u ()
arrowdblboth    = symbEscInt 0o333

arrowdbldown    :: Num u => LRText u ()
arrowdbldown    = symbEscInt 0o337

arrowdblleft    :: Num u => LRText u ()
arrowdblleft    = symbEscInt 0o334

arrowdblright   :: Num u => LRText u ()
arrowdblright   = symbEscInt 0o336

arrowdblup      :: Num u => LRText u ()
arrowdblup      = symbEscInt 0o335

arrowdown       :: Num u => LRText u ()
arrowdown       = symbEscInt 0o257

arrowleft       :: Num u => LRText u ()
arrowleft       = symbEscInt 0o254

arrowright      :: Num u => LRText u ()
arrowright      = symbEscInt 0o256

arrowup         :: Num u => LRText u ()
arrowup         = symbEscInt 0o255

asteriskmath    :: Num u => LRText u ()
asteriskmath    = symbEscInt 0o052

bar             :: Num u => LRText u ()
bar             = symbEscInt 0o174

braceleft       :: Num u => LRText u ()
braceleft       = symbEscInt 0o173

braceright      :: Num u => LRText u ()
braceright      = symbEscInt 0o175

bracketleft     :: Num u => LRText u ()
bracketleft     = symbEscInt 0o133

bracketright    :: Num u => LRText u ()
bracketright    = symbEscInt 0o135

bullet          :: Num u => LRText u ()
bullet          = symbEscInt 0o267

carriagereturn  :: Num u => LRText u ()
carriagereturn  = symbEscInt 0o277


circlemultiply  :: Num u => LRText u ()
circlemultiply  = symbEscInt 0o304

circleplus      :: Num u => LRText u ()
circleplus      = symbEscInt 0o305

club            :: Num u => LRText u ()
club            = symbEscInt 0o247

colon           :: Num u => LRText u ()
colon           = symbEscInt 0o072

comma           :: Num u => LRText u ()
comma           = symbEscInt 0o054

congruent       :: Num u => LRText u ()
congruent             = symbEscInt 0o100

copyrightsans   :: Num u => LRText u ()
copyrightsans   = symbEscInt 0o343

copyrightserif  :: Num u => LRText u ()
copyrightserif  = symbEscInt 0o323

degree          :: Num u => LRText u ()
degree          = symbEscInt 0o260

diamond         :: Num u => LRText u ()
diamond         = symbEscInt 0o250

divide          :: Num u => LRText u ()
divide          = symbEscInt 0o270

dotmath         :: Num u => LRText u ()
dotmath         = symbEscInt 0o327

eight           :: Num u => LRText u ()
eight           = symbEscInt 0o070

element         :: Num u => LRText u ()
element         = symbEscInt 0o316

ellipsis        :: Num u => LRText u ()
ellipsis        = symbEscInt 0o274

emptyset        :: Num u => LRText u ()
emptyset        = symbEscInt 0o306

equal           :: Num u => LRText u ()
equal           = symbEscInt 0o075

equivalence     :: Num u => LRText u ()
equivalence     = symbEscInt 0o272

exclam          :: Num u => LRText u ()
exclam          = symbEscInt 0o041

existential     :: Num u => LRText u ()
existential     = symbEscInt 0o044

five            :: Num u => LRText u ()
five            = symbEscInt 0o065

florin          :: Num u => LRText u ()
florin          = symbEscInt 0o246

four            :: Num u => LRText u ()
four            = symbEscInt 0o064

fraction        :: Num u => LRText u ()
fraction        = symbEscInt 0o244

gradient        :: Num u => LRText u ()
gradient        = symbEscInt 0o321

greater         :: Num u => LRText u ()
greater         = symbEscInt 0o076

greaterequal    :: Num u => LRText u ()
greaterequal    = symbEscInt 0o263

heart           :: Num u => LRText u ()
heart           = symbEscInt 0o251

infinity        :: Num u => LRText u ()
infinity        = symbEscInt 0o245

integral        :: Num u => LRText u ()
integral        = symbEscInt 0o362

intersection    :: Num u => LRText u ()
intersection    = symbEscInt 0o307

less            :: Num u => LRText u ()
less            = symbEscInt 0o074

lessequal       :: Num u => LRText u ()
lessequal       = symbEscInt 0o243

logicaland      :: Num u => LRText u ()
logicaland      = symbEscInt 0o331

logicalnot      :: Num u => LRText u ()
logicalnot      = symbEscInt 0o330

logicalor       :: Num u => LRText u ()
logicalor       = symbEscInt 0o332

lozenge         :: Num u => LRText u ()
lozenge         = symbEscInt 0o340

minus           :: Num u => LRText u ()
minus           = symbEscInt 0o055

minute          :: Num u => LRText u ()
minute          = symbEscInt 0o242

multiply        :: Num u => LRText u ()
multiply        = symbEscInt 0o264

nine            :: Num u => LRText u ()
nine            = symbEscInt 0o071

notelement      :: Num u => LRText u ()
notelement      = symbEscInt 0o317

notequal        :: Num u => LRText u ()
notequal        = symbEscInt 0o271

notsubset       :: Num u => LRText u ()
notsubset       = symbEscInt 0o313

numbersign      :: Num u => LRText u ()
numbersign      = symbEscInt 0o043

omega1          :: Num u => LRText u ()
omega1          = symbEscInt 0o166

omicron         :: Num u => LRText u ()
omicron         = symbEscInt 0o157

one             :: Num u => LRText u ()
one             = symbEscInt 0o061

parenleft       :: Num u => LRText u ()
parenleft       = symbEscInt 0o050

parenright      :: Num u => LRText u ()
parenright      = symbEscInt 0o051

partialdiff     :: Num u => LRText u ()
partialdiff     = symbEscInt 0o266

percent         :: Num u => LRText u ()
percent         = symbEscInt 0o045

period          :: Num u => LRText u ()
period          = symbEscInt 0o056

perpendicular   :: Num u => LRText u ()
perpendicular   = symbEscInt 0o136

phi1            :: Num u => LRText u ()
phi1            = symbEscInt 0o152

plus            :: Num u => LRText u ()
plus            = symbEscInt 0o053

plusminus       :: Num u => LRText u ()
plusminus       = symbEscInt 0o261

product         :: Num u => LRText u ()
product         = symbEscInt 0o325

propersubset    :: Num u => LRText u ()
propersubset    = symbEscInt 0o314

propersuperset  :: Num u => LRText u ()
propersuperset  = symbEscInt 0o311

proportional    :: Num u => LRText u ()
proportional    = symbEscInt 0o265

question        :: Num u => LRText u ()
question        = symbEscInt 0o077

radical         :: Num u => LRText u ()
radical         = symbEscInt 0o326

radicalex       :: Num u => LRText u ()
radicalex       = symbEscInt 0o140

reflexsubset    :: Num u => LRText u ()
reflexsubset    = symbEscInt 0o315

reflexsuperset  :: Num u => LRText u ()
reflexsuperset  = symbEscInt 0o312

registersans    :: Num u => LRText u ()
registersans    = symbEscInt 0o342

registerserif   :: Num u => LRText u ()
registerserif   = symbEscInt 0o322

second          :: Num u => LRText u ()
second          = symbEscInt 0o262

semicolon       :: Num u => LRText u ()
semicolon       = symbEscInt 0o073

seven           :: Num u => LRText u ()
seven           = symbEscInt 0o067

sigma1          :: Num u => LRText u ()
sigma1          = symbEscInt 0o126

similar         :: Num u => LRText u ()
similar         = symbEscInt 0o176

six             :: Num u => LRText u ()
six             = symbEscInt 0o066

slash           :: Num u => LRText u ()
slash           = symbEscInt 0o057

space           :: Num u => LRText u ()
space           = symbEscInt 0o040

spade           :: Num u => LRText u ()
spade           = symbEscInt 0o252

suchthat        :: Num u => LRText u ()
suchthat        = symbEscInt 0o047

summation       :: Num u => LRText u ()
summation       = symbEscInt 0o345

therefore       :: Num u => LRText u ()
therefore       = symbEscInt 0o134

theta1          :: Num u => LRText u ()
theta1          = symbEscInt 0o112

three           :: Num u => LRText u ()
three           = symbEscInt 0o063

trademarksans   :: Num u => LRText u ()
trademarksans   = symbEscInt 0o344

trademarkserif  :: Num u => LRText u ()
trademarkserif  = symbEscInt 0o324

two             :: Num u => LRText u ()
two             = symbEscInt 0o062

underscore      :: Num u => LRText u ()
underscore      = symbEscInt 0o137

union           :: Num u => LRText u ()
union           = symbEscInt 0o310

universal       :: Num u => LRText u ()
universal       = symbEscInt 0o042

weierstrass     :: Num u => LRText u ()
weierstrass     = symbEscInt 0o303

zero            :: Num u => LRText u ()
zero            = symbEscInt 0o060


