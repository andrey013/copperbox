{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Duration
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Duration representation.
--
--------------------------------------------------------------------------------



module Mullein.Duration where


import Data.Monoid
import Data.Ratio


data Numeral = N128  | N64   | N32   | N16
             | N8    | N4    | N2    | N1
             | Breve | Longa
  deriving (Eq,Ord)


-- numeral x dot count
data ND = ND Numeral Int
 deriving Eq


data Duration = D0
              | D1 ND
              | Dn [ND]
  deriving Eq


instance Ord ND where
  compare d1 d2 = sumND d1 `compare` sumND d2

instance Ord Duration where
  compare d1 d2 = extent d1 `compare` extent d2


instance Show Duration where
  show = ('#' :) . show . extent



instance Monoid Duration where
  mempty  = D0
  mappend = (#+)


class HasDuration a where
  getDuration  :: a -> Duration
  setDuration  :: Duration -> a -> a

instance HasDuration Duration where
  getDuration  = id
  setDuration  = const


class Spacer a where
  spacer :: Duration -> a

toRat :: Numeral -> Rational
toRat N128  = 1%128
toRat N64   = 1%64
toRat N32   = 1%32
toRat N16   = 1%16
toRat N8    = 1%8
toRat N4    = 1%4
toRat N2    = 1%2
toRat N1    = 1
toRat Breve = 2
toRat Longa = 4

-- Zero durations do exist (the duration of a grace notes is officially
-- zero), however we ought not to be able to construct them. 
isZero :: Duration -> Bool
isZero D0 = True
isZero _    = False

dZero :: Duration
dZero = D0

-- | Composite durations have more than 1 component typically they
-- will be represented as tied notes.
isComposite :: Duration -> Bool
isComposite (Dn _) = True 
isComposite _      = False

isDotted :: Duration -> Bool
isDotted D0             = False 
isDotted (D1 (ND _ dc)) = dc>0
isDotted (Dn xs)        = any isDot1 xs
  where
    isDot1 (ND _ d) = d>0


components :: Duration -> [(Rational,Int)]
components D0             = []
components (D1 (ND n dc)) = [(toRat n,dc)]
components (Dn xs)        = map (\(ND n dc) -> (toRat n,dc)) xs


-- | @extent@ is the sum of all symbolic components of a duration.
extent :: Duration -> Rational
extent D0 = 0 
extent (D1 x) = sumND x
extent (Dn xs)  = foldr (\a s -> s + sumND a) 0 xs



sumND :: ND -> Rational
sumND (ND n dc) | dc <= 0  = toRat n
                | otherwise = let r = toRat n in step r (r/2) dc
  where
    step acc _ 0 = acc
    step acc h i = step (acc + h) (h/2) (i-1)


-- | Add (concatenate) two durations. 
-- Addition (concatenation) produces a symbolic value equivalent 
-- to the two durations being tied: @1/4 #+ 1/4 = 1/4~1/4@.
-- It does not perform numeric addition: @1/4 #+ 1/4 /= 1/2@.
(#+) :: Duration -> Duration -> Duration
D0     #+ b      = b
a      #+ D0     = a
D1 a   #+ D1 b   = Dn [a,b]
D1 a   #+ Dn ys  = Dn (a:ys)
Dn xs  #+ D1 y   = Dn $ xs ++ [y]
Dn xs  #+ Dn ys  = Dn $ xs ++ ys

       
-- | Dot a duration. 
-- Note, if the duration represents a concatenation of two or more 
-- primitive durations only the first will be dotted.
dot :: Duration -> Duration
dot D0              = error "Duration.dot - cannot dot 0 duration"
dot (D1 (ND n dc))  = D1 $ ND n (dc+1)
dot (Dn xs)         = Dn $ first dot1 xs 
  where
    dot1 (ND n dc)    = ND n (dc+1)
    first f (y:ys)    = f y:ys
    first _ []        = error $ "Duration.dot - bad list"  -- unreachable (?)
  

splitDuration :: Rational -> Duration -> (Maybe Duration, Duration)
splitDuration _     D0                  = (Nothing,D0)
splitDuration _  d@(D1 _)               = (Nothing,d)
splitDuration r0   (Dn xs)              = splitls r0 xs
  where 
    splitls _ []     = error "Duration.splitDuration empty" -- unreachable (?)
    splitls _ [y]    = (Nothing, D1 y) -- cannot exhaust righthand side
    splitls r (y:ys) = let d1 = sumND y in case compare r d1 of
                          LT -> jcons y (splitls (r-d1) ys)
                          EQ -> (Just $ D1 y, Dn ys)
                          GT -> (Nothing, Dn (y:ys))
    jcons x (Nothing,drest) = (Just $ D1 x, drest)
    jcons x (Just lf,drest) = (Just $ (D1 x #+ lf), drest)            




lilypond :: Duration -> [(Either String Int,Int)]
lilypond D0             = []
lilypond (D1 (ND n dc)) = [(numeralLy n, dc)] 
lilypond (Dn xs)        = map (\(ND n dc) -> (numeralLy n,dc)) xs

numeralLy :: Numeral -> Either String Int
numeralLy N128  = Right 128
numeralLy N64   = Right 64
numeralLy N32   = Right 32
numeralLy N16   = Right 16
numeralLy N8    = Right 8
numeralLy N4    = Right 4
numeralLy N2    = Right 2
numeralLy N1    = Right 1
numeralLy Breve = Left "breve"
numeralLy Longa = Left "longa"

data AbcMultiplier = Unit | Mult Integer | Div Integer | Frac Integer Integer
  deriving (Eq,Show)

abc :: Rational -> Duration -> [AbcMultiplier]
abc _   D0      = [] 
abc unl (D1 nd) = [multiplierABC unl nd]
abc unl (Dn xs) = map (multiplierABC unl) xs

multiplierABC :: Rational -> ND -> AbcMultiplier
multiplierABC unl nd = (fn . fork numerator denominator) $ (sumND nd) / unl
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = Unit
    fn (1,dn)  = Div dn
    fn (nm,1)  = Mult nm
    fn (nm,dn) = Frac nm dn

 
mkDuration :: Numeral -> Duration
mkDuration nm = D1 $ ND nm 0


longa :: Duration
breve :: Duration
wn    :: Duration
hn    :: Duration
qn    :: Duration
en    :: Duration
sn    :: Duration
tn    :: Duration


longa = mkDuration Longa
breve = mkDuration Breve
wn    = mkDuration N1
hn    = mkDuration N2
qn    = mkDuration N4
en    = mkDuration N8
sn    = mkDuration N16
tn    = mkDuration N32

dhn   :: Duration
dqn   :: Duration
den   :: Duration
dsn   :: Duration

dhn   = dot hn
dqn   = dot qn 
den   = dot en 
dsn   = dot sn



