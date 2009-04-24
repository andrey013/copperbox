{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.System
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  Flexible instances, mptc.
--
-- An interface for Haskore to Mullein
--
--------------------------------------------------------------------------------


module MulleinHaskore.System where

-- import MulleinHaskore.MusicFold

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch
import Mullein.Score 

import qualified Haskore.Basics      as H

import Data.List ( foldl' )
import qualified Data.Map as M
import Data.Ratio
import Data.Sequence hiding ( null )
import qualified Data.Sequence as S


-- Note Munz converts Music not Perfomance...

type System e = M.Map H.IName e 

system :: H.Music -> System e
system _ = undefined


melody :: Key -> MetricalSpec -> [Element] -> Part
melody k m xs = part [p1] where 
    p1 = phrase $ motif k m (primary xs) 


-- Get out of the Hudak tree as quickly as possible...

data HEvent = HskNote H.Pitch H.Dur [H.NoteAttribute]
            | HskChord [(H.Pitch,H.NoteAttribute)] H.Dur
            | HskRest H.Dur
            | HskPhraseMarks [H.PhraseAttribute]
  deriving (Eq,Show)

-- put instrname into MLine...

doodle :: H.IName -> System [(H.Dur,Seq HEvent)] -> [(Duration,Cardinal HEvent)]
doodle instr sys = mergeOnsets $ map onset $ maybe [] id (M.lookup instr sys)



{-
extractOverlays :: [(H.Dur,Cardinal HEvent)] -> [[Element]]
extractOverlays (start,xs) | start == 0 = step 
-}

overlay1 :: Duration 
         -> [(Duration,Cardinal HEvent)] 
         -> ([Element],[(Duration,Cardinal HEvent)])
overlay1 _   []          = ([],[])
overlay1 now ((t,e):xs)  
         | t < now       = let (es,rst)  = overlay1 now xs in (es,(t,e):rst)
         | t == now      = let e'        = mkElem e
                               (es,rst)  = overlay1 (now+duration e') xs
                           in (e':es,rst)
         | otherwise     = let s'        = spacer $ t - now
                               (es,rst)  = overlay1 t ((t,e):xs)
                           in (s':es,rst)


mkElem :: Cardinal HEvent -> Element
mkElem (Left _) = undefined
mkElem (Right xs) = undefined


-- Finding chords in Haskore is tedious as they have no 
-- /structural/ identification (i.e. there is no Chord constructor).
-- We Look for notes with the same onset time and duration...

type Cardinal a = Either a [a]

mergeOnsets :: [[(Duration,HEvent)]] -> [(Duration,Cardinal HEvent)] 
mergeOnsets (xs:ys:yss) = foldl' merge cs yss where 
                            cs = merge1 xs ys
mergeOnsets [xs]        = map (\(d,e) -> (d,Left e)) xs
mergeOnsets []          = []


merge1 :: [(Duration,HEvent)] -> [(Duration,HEvent)] -> [(Duration,Cardinal HEvent)]
merge1 = cardinalOnsetMerge1 compare hskDurEq  

merge :: [(Duration,Cardinal HEvent)] -> [(Duration,HEvent)] -> [(Duration,Cardinal HEvent)]
merge = cardinalOnsetMerge compare hskDurEq  


hskDurEq :: HEvent -> HEvent -> Bool
hskDurEq a b = hskDur a == hskDur b



cardinalOnsetMerge1 :: (a -> a -> Ordering)
                   -> (b -> b -> Bool) 
                   -> [(a,b)] 
                   -> [(a,b)] 
                   -> [(a,Cardinal b)]
cardinalOnsetMerge1 cmp pf = step where
    step (x:xs) (y:ys) = case cmp (fst x) (fst y) of
                                     LT -> cLeft x : step xs (y:ys)
                                     GT -> cLeft y : step (x:xs) ys
                                     EQ -> if pf (snd x) (snd y) 
                                             then cRight x y : step xs ys
                                             else cLeft x : cLeft y : step xs ys
    step []     (y:ys) = cLeft y : step [] ys
    step (x:xs) []     = cLeft x : step xs []
    step []     []     = []

    cLeft (a,b) = (a, Left b)
    cRight (a,b) (_,b') = (a, Right [b,b'])  


-- variant that merges into a cardinal list
cardinalOnsetMerge :: (a -> a -> Ordering)
                   -> (b -> b -> Bool) 
                   -> [(a,Cardinal b)] 
                   -> [(a,b)] 
                   -> [(a,Cardinal b)]
cardinalOnsetMerge cmp pf = step where
    step (x:xs) (y:ys) = case cmp (fst x) (fst y) of
                                     LT -> x : step xs (y:ys)
                                     GT -> cLeft y : step (x:xs) ys
                                     EQ -> if pf (csnd x) (snd y) 
                                             then cRight x y : step xs ys
                                             else x : cLeft y : step xs ys
    step []     (y:ys) = cLeft y : step [] ys    
    step xs     []     = xs  -- left list is already in cardinal form so flush it

    cLeft (a,b) = (a, Left b)
    cRight (a,Left b)   (_,b') = (a, Right [b,b'])  
    cRight (a,Right xs) (_,b') = (a, Right (xs++[b']))

    csnd (_,Left a)       = a
    csnd (_,Right (x:_))  = x
    csnd (_,Right [])     = error $ "ill-made cardinal - how did that happen?"







onset :: (H.Dur, Seq HEvent) -> [(Duration,HEvent)]
onset (start,ese) = step (cDur start) (viewl ese) where
    step _ EmptyL = []
    step o (evt :< sa)     = (o,evt): step (o + (cDur $ hskDur evt)) (viewl sa)

hskDur :: HEvent -> H.Dur
hskDur (HskNote _ d _)     = d
hskDur (HskChord _ d)      = d
hskDur (HskRest d)         = d
hskDur (HskPhraseMarks _)  = 0

hskSystem :: [MLine] -> System [(H.Dur,Seq HEvent)]
hskSystem = foldr f M.empty where
  f (o,name,se) m = maybe (M.insert name [(o,se)] m)
                          (\xs -> M.insert name ((o,se):xs) m)
                          (M.lookup name m)

type MLine = (H.Dur, H.IName, Seq HEvent)

flattenMusic :: H.IName -> H.Music -> [MLine]
flattenMusic instr =  snd . untree 0 instr

untree :: H.Dur -> H.IName -> H.Music -> (H.Dur, [MLine])
untree start instr = step start (start,instr,empty) []
  where
    step :: H.Dur -> MLine -> [MLine] -> H.Music -> (H.Dur, [MLine])
    step t z zs (H.Note p d xs)     = (t+d, z `snoc` (HskNote p d xs):zs)

    step t z zs (H.Rest d)          = (t+d, z `snoc` (HskRest d):zs)


    step t z zs (lhs H.:+: rhs)     = step t' z' zs' rhs
                                      where (t',z':zs') = step t z zs lhs
    
    step t z zs (lhs H.:=: rhs)     = step t z (zs++zs') lhs
                                      where (_,zs') = untree t instr rhs

    step t z zs (H.Tempo _ mus)     = step t z zs mus

    step t z zs (H.Trans _ mus)     = step t z zs mus

    step t z zs (H.Instr nom mus)   = (t,z `add` (zs++zs')) where 
                                        zs' = snd $ untree t nom mus

                                        add (d,nm,se) xs 
                                            | S.null se = xs
                                            | otherwise = (d,nm,se):xs

    step t z zs (H.Player _ mus)    = step t z zs mus
    
    step t z zs (H.Phrase xs mus)   = step t (z `snoc` HskPhraseMarks xs) zs mus

    snoc (t,name,se) e = (t,name,se |> e)

{-
instrStart :: H.IName -> MLine -> MLine
instrStart n (t,se) = case viewl se of
                        (HskInstr _ :< _) -> (t,se)
                        _                 -> (t,HskInstr n <| se)
-}                           



                        
cPitch :: H.Pitch -> Pitch
cPitch (H.Cf, o)    = Pitch C Flat o
cPitch (H.C,  o)    = Pitch C Nat o
cPitch (H.Cs, o)    = Pitch C Sharp o
cPitch (H.Df, o)    = Pitch D Flat o
cPitch (H.D,  o)    = Pitch D Nat o
cPitch (H.Ds, o)    = Pitch D Sharp o
cPitch (H.Ef, o)    = Pitch E Flat o
cPitch (H.E,  o)    = Pitch E Nat o
cPitch (H.Es, o)    = Pitch E Sharp o
cPitch (H.Ff, o)    = Pitch F Flat o
cPitch (H.F,  o)    = Pitch F Nat o
cPitch (H.Fs, o)    = Pitch F Sharp o
cPitch (H.Gf, o)    = Pitch G Flat o
cPitch (H.G,  o)    = Pitch G Nat o
cPitch (H.Gs, o)    = Pitch G Sharp o
cPitch (H.Af, o)    = Pitch A Flat o
cPitch (H.A,  o)    = Pitch A Nat o
cPitch (H.As, o)    = Pitch A Sharp o
cPitch (H.Bf, o)    = Pitch B Flat o
cPitch (H.B,  o)    = Pitch B Nat o
cPitch (H.Bs, o)    = Pitch B Sharp o


cDur :: H.Dur -> Duration
cDur r = n%d where 
    n = fromIntegral $ numerator r
    d = fromIntegral $ denominator r 

{-


-- Rests have been lost in Haskore performance
-- (Trills will be very difficult to recover...)



type System =  [(H.IName, [Element])]



system' :: H.Music -> System
system' = system . H.testPerf

system :: H.Performance -> System
system perf = let split_list = splitByInst perf
               in map (\(n,p) -> (show n, instEL (n,p))) split_list


melody :: Key -> MetricalSpec -> [Element] -> Part
melody k m xs = part [p1] where 
    p1 = phrase $ motif k m (primary xs) 



instrumentNames :: H.Performance -> [String]
instrumentNames = map (show . fst) . splitByInst

  
instEL :: (H.IName,H.Performance) -> [Element]
instEL (_,p) = reverse $ snd $ foldl fn (0,[]) $ groupChords p
  where
    fn (onset,tree) e   = let (e_onset, e_dur, el) = evt e in 
        if e_onset == onset
          then (e_onset + e_dur, el : tree)
          else let r = convert (e_onset - onset)
               in (e_onset + e_dur, rest r : el : tree)

groupChords :: [H.Event] -> [[H.Event]]
groupChords = groupBy (\a b -> H.eTime a == H.eTime b)

    
evt :: [H.Event] -> (H.Time, H.DurT, Element)
evt [e]     = event1 e
evt (e:es)  = (e_onset, e_dur, chord chord_notes (convert e_dur)) where
                  chord_notes = sort $ map convert (e:es)
                  e_dur       = H.eDur e
                  e_onset     = H.eTime e
evt []      = error $ "evt - empty list"

          
event1 :: H.Event -> (H.Time, H.DurT, Element)
event1 e@(H.Event {H.eTime=onset, H.eDur=drn}) = 
    (onset,drn, note (convert e) (convert drn))


-- From Haskore, ToMidi.lhs -- not an exposed function
splitByInst :: H.Performance ->  [(H.IName,H.Performance)]
splitByInst [] = []
splitByInst pf = (i,pf1) : splitByInst pf2
  where i         = H.eInst (head pf)
        (pf1,pf2) = partition (\e -> H.eInst e == i) pf
    




class Convert a b where
  convert :: a -> b 


instance Convert H.Event Pitch where
  convert (H.Event {H.ePitch = p}) = convert p

-- This is introducing problems with pitch spelling...

instance Convert H.AbsPitch Pitch where
  convert = fromSemitones


instance Convert H.DurT Duration where
  convert d = error $ show d

instance Convert H.Dur Duration where
  convert r = let (n,d) = (numerator r, denominator r)
              in (fromIntegral n % fromIntegral d)

instance Convert (H.PitchClass,Int) Pitch where
  convert (H.Cf, o)    = Pitch C Flat o
  convert (H.C,  o)    = Pitch C Nat o
  convert (H.Cs, o)    = Pitch C Sharp o
  convert (H.Df, o)    = Pitch D Flat o
  convert (H.D,  o)    = Pitch D Nat o
  convert (H.Ds, o)    = Pitch D Sharp o
  convert (H.Ef, o)    = Pitch E Flat o
  convert (H.E,  o)    = Pitch E Nat o
  convert (H.Es, o)    = Pitch E Sharp o
  convert (H.Ff, o)    = Pitch F Flat o
  convert (H.F,  o)    = Pitch F Nat o
  convert (H.Fs, o)    = Pitch F Sharp o
  convert (H.Gf, o)    = Pitch G Flat o
  convert (H.G,  o)    = Pitch G Nat o
  convert (H.Gs, o)    = Pitch G Sharp o
  convert (H.Af, o)    = Pitch A Flat o
  convert (H.A,  o)    = Pitch A Nat o
  convert (H.As, o)    = Pitch A Sharp o
  convert (H.Bf, o)    = Pitch B Flat o
  convert (H.B,  o)    = Pitch B Nat o
  convert (H.Bs, o)    = Pitch B Sharp o




-}

