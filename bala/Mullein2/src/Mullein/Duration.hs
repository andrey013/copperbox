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

import Data.OneMany

import Data.Monoid
import Data.Ratio

-- rational duration x dot count
newtype D1 = D1 { getComponent :: (Numeral,Int) }
 deriving Eq


data Numeral = N128  | N64   | N32   | N16
             | N8    | N4    | N2    | N1
             | Breve | Longa
  deriving (Eq,Ord)



data Duration = DZero 
              | Dn (OneMany D1)
  deriving Eq


instance Ord D1 where
  compare d1 d2 = sumD1 d1 `compare` sumD1 d2

instance Ord Duration where
  compare d1 d2 = extent d1 `compare` extent d2


instance Show Duration where
  show = ('#' :) . show . extent



instance Monoid Duration where
  mempty  = DZero
  mappend = (#+)

{-
  a     `mappend` DZero = a
  DZero `mappend` b     = b
  a     `mappend
                | isZero b  = a
                | otherwise = a #+ b   
-}

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
isZero DZero = True
isZero _    = False

dZero :: Duration
dZero = DZero

-- | Composite durations have more than 1 component typically they
-- will be represented as tied notes.
isComposite :: Duration -> Bool
isComposite DZero   = False
isComposite (Dn om) = isOne om 



components :: Duration -> [(Rational,Int)]
components DZero   = []
components (Dn om) = oneMany (wrap . un . getComponent) (map (un . getComponent)) om 
  where 
    wrap a = [a]
    un (nm,i) = (toRat nm,i)
   


-- | @extent@ is the sum of all symbolic components of a duration.
extent :: Duration -> Rational
extent DZero = 0 
extent (Dn xs) = oneMany sumD1 (foldr (\a s -> s + sumD1 a) 0) xs



sumD1 :: D1 -> Rational
sumD1 (D1 (nm,i)) | i <= 0 = toRat nm
sumD1 (D1 (nm,i))          = let r = toRat nm in step r (r/2) i where
  step acc _ 0 = acc
  step acc h n = step (acc + h) (h/2) (n-1)


-- | Add (concatenate) two durations. 
-- Addition (concatenation) produces a symbolic value equivalent 
-- to the two durations being tied: @1/4 #+ 1/4 = 1/4~1/4@.
-- It does not perform numeric addition: @1/4 #+ 1/4 /= 1/2@.
(#+) :: Duration -> Duration -> Duration
DZero  #+ b      = b
a      #+ DZero  = a
Dn oml #+ Dn omr = Dn $ foldr cons omr (toList oml)

       
-- | Dot a duration. 
-- Note, if the duration represents a concatenation of two or more 
-- primitive durations only the first will be dotted.
dot :: Duration -> Duration
dot DZero = error "Duration.dot - cannot dot 0 duration"
dot (Dn om) = Dn $ oneMany (one . dot1) (many . first dot1) om
  where
    dot1 (D1 (a,n))   = D1 (a,n+1)
    first f (x:xs)  = f x:xs
    first _ []      = error $ "Duration.dot - bad list"  -- should be unreachable in this case
  

splitDuration :: Rational -> Duration -> (Maybe Duration, Duration)
splitDuration _   DZero = (Nothing,DZero)
splitDuration r d@(Dn om) | isOne om  = (Nothing,d)
                          | otherwise = splitls r (toList om)
  where 
    splitls _ [x]    = (Nothing, Dn $ one x) -- cannot exhaust righthand side
    splitls r (x:xs) = let d1 = sumD1 x in case compare r d1 of
                          LT -> jcons x (splitls (r-d1) xs)
                          EQ -> (Just $ Dn $ one x, Dn $ fromList xs)
                          GT -> (Nothing, Dn $ fromList (x:xs))
    jcons x (Nothing,drest) = (Just $ Dn $ one x, drest)
    jcons x (Just lf,drest) = (Just $ (Dn $ one x) #+ lf, drest)            




lilypond :: Duration -> [(Either String Int,Int)]
lilypond DZero   = []
lilypond (Dn om) = map (prod fn id . getComponent) $ toList om where
  prod f g (a,b) = (f a, g b)
  fn N128  = Right 128
  fn N64   = Right 64
  fn N32   = Right 32
  fn N16   = Right 16
  fn N8    = Right 8
  fn N4    = Right 4
  fn N2    = Right 2
  fn N1    = Right 1
  fn Breve = Left "breve"
  fn Longa = Left "longa"

data AbcMultiplier = Unit | Mult Integer | Div Integer | Frac Integer Integer
  deriving (Eq,Show)

abc :: Rational -> Duration -> [AbcMultiplier]
abc _   DZero   = [] 
abc unl (Dn om) = map (fn . fork numerator denominator . (/unl) . sumD1) $ toList om
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = Unit
    fn (1,dn)  = Div dn
    fn (nm,1)  = Mult nm
    fn (nm,dn) = Frac nm dn

 
mkDuration :: Numeral -> Duration
mkDuration nm = Dn $ one $ D1 (nm,0)


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

dhn :: Duration
dqn :: Duration
den :: Duration
dsn :: Duration

dhn = dot hn
dqn = dot qn 
den = dot en 
dsn = dot sn



