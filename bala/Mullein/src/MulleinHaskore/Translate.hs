{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
 
--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.Translate
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Translate Haskore's Music datatype to Mullein scores for each instrument.
--
--------------------------------------------------------------------------------


module MulleinHaskore.Translate where

import MulleinHaskore.Rewriting
import MulleinHaskore.StringRewriting

import Mullein.Core ( OverlayList, Key, MetricalSpec )
import Mullein.Duration
import Mullein.SpellingMap

import qualified Mullein.Pitch       as M
import qualified Mullein.Score       as M

import qualified Haskore.Basics      as H

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader

import qualified Data.Foldable as F
import Data.List ( sortBy, foldl' )
import Data.Ratio
import Data.Sequence hiding ( null )
import qualified Data.Sequence as S

import qualified Text.PrettyPrint.Leijen as PP


type OnsetTime = Duration
type AlphElem = Alphabet M.ScNote

type AlphLine  = (OnsetTime, [AlphElem])
type SAlphLine = (OnsetTime, Seq AlphElem)

type ElemLine  = (OnsetTime, [M.Element])

type TranspDist = Int

-- Note - extract just the 'music' not the onset
-- music may have been transposed...


findInst :: H.IName -> H.Music -> Maybe (TranspDist, H.Music)
findInst name skore = step 0 skore
  where
    step t (H.Instr nm mus) 
      | nm==name              = maybe Nothing (\m -> Just (t,m)) $ 
                                      dropInstrBranches name mus
      | otherwise             = step t mus
    
    step _ (H.Note _ _ _)     = Nothing
    step _ (H.Rest _)         = Nothing

    step t (lhs H.:+: rhs)    = step t lhs `mfirst` step t rhs


    step t (lhs H.:=: rhs)    = step t lhs `mfirst` step t rhs

    step t (H.Tempo _ mus)    = step t mus

    step t (H.Trans i mus)    = step (t+i) mus

    step t (H.Player _ mus)   = step t mus
    
    step t (H.Phrase _ mus)   = step t mus
      

mfirst :: Maybe a -> Maybe a -> Maybe a
mfirst (Just a) _       = Just a
mfirst _        b       = b
  

-- Note - drops Player, Phrase and Tempo constructors

dropInstrBranches :: H.IName -> H.Music -> Maybe H.Music
dropInstrBranches name skore= step skore 
  where
    step (H.Instr nm mus)     
       | nm == name           = step mus
       | otherwise            = Nothing
    step (H.Note p d xs)      = Just $ H.Note p d xs
    step (H.Rest d)           = Just $ H.Rest d
    step (lhs H.:+: rhs)      = step lhs `mbSeq` step rhs
    step (lhs H.:=: rhs)      = step lhs `mbPar` step rhs
    step (H.Tempo _ mus)      = step mus
    step (H.Trans i mus)      = maybe Nothing sk $ step mus
                                where 
                                  sk body = Just $ H.Trans i body
    step (H.Player _ mus)     = step mus
    step (H.Phrase _ mus)     = step mus

 

mbSeq :: Maybe H.Music -> Maybe H.Music -> Maybe H.Music
mbSeq (Just l) (Just r) = Just $ l H.:+: r
mbSeq Nothing  b        = b
mbSeq a        Nothing  = a

mbPar :: Maybe H.Music -> Maybe H.Music -> Maybe H.Music
mbPar (Just l) (Just r) = Just $ l H.:=: r
mbPar Nothing  b        = b
mbPar a        Nothing  = a


--------------------------------------------------------------------------------


type TransposeM a = Reader Env a

data Env = Env { transp :: TranspDist, spelling :: SpellingMap }

instance Applicative (Reader Env) where
  pure  = return
  (<*>) = ap

instrLines :: TranspDist -> SpellingMap -> H.Music -> [AlphLine]
instrLines n smap skore = 
    sortBy onsetCmp $ fn $ runReader (untree 0 skore) env
  where
    env = Env { transp = n, spelling = smap }

    fn (_,(o,se),xss) = (o, F.toList se) : xss
    
    onsetCmp (o,_) (o',_) = o `compare` o'
 
untree :: Duration -> H.Music -> TransposeM (Duration, SAlphLine, [AlphLine])
untree start skore = step start (start,S.empty) [] skore
  where
    step :: Duration -> SAlphLine -> [AlphLine] -> H.Music 
         -> TransposeM (Duration, SAlphLine, [AlphLine])
    step t z zs (H.Note p d xs)     = (\nt -> (t + cDur d, z `snoc` nt, zs))  
                                        <$> mkANote p xs d


    step t z zs (H.Rest d)          = return (t+d', z `snoc` arest d', zs)
                                      where d' = cDur d


    step t z zs (lhs H.:+: rhs)     = do { (t',z',zs') <- step t z zs lhs
                                         ; step t' z' zs' rhs }


    step t z zs (lhs H.:=: rhs)     = do { (_,z',zs') <- untree t rhs
                                         ; step t z (zs++ (sline z':zs')) lhs }

    step t z zs (H.Tempo _ mus)     = step t z zs mus

    step t z zs (H.Trans i mus)     = do { j <- asks transp
                                         ; local (\e -> e {transp=j+i}) 
                                                 (step t z zs mus) }


    step t z zs (H.Instr _ _)       = return (t,z,zs)

    step t z zs (H.Player _ mus)    = step t z zs mus
    
    step t z zs (H.Phrase _ mus)    = step t z zs mus

    snoc (t,se) e = (t,se |> e)

sline :: (Duration, Seq AlphElem) -> (Duration, [AlphElem])
sline (d,se) = (d,F.toList se)


mkANote :: H.Pitch -> [H.NoteAttribute] -> H.Dur -> TransposeM AlphElem
mkANote p _xs d = (\t smap -> anote (fn p t smap) [] (cDur d))
                   <$> asks transp <*> asks spelling
  where 
    fn pch i smap = rename smap (M.transpose i (cPitch pch))


--------------------------------------------------------------------------------
-- rewrite and turn into Mullein overlays

rewriteLines :: Monad m 
             => (m [AlphElem] -> [AlphElem]) 
             -> RuleTP AlphElem m 
             -> [AlphLine] 
             -> [AlphLine]
rewriteLines run rw = map rwline
  where
   rwline (d,xs) = (d, run $ rewriteTP rw xs)


rewriteLines'Id :: RuleTP AlphElem Identity -> [AlphLine] -> [AlphLine]
rewriteLines'Id = rewriteLines runIdentity


alphLinesToElemLines :: [AlphLine] -> [ElemLine]
alphLinesToElemLines = map rwline
  where
    rwline (d,xs) = (d, alphabetToElementP xs)
 

makeOverlays :: [ElemLine] -> OverlayList M.ScNote
makeOverlays []     = ([],[])
makeOverlays (x:xs) = foldl' fn (M.primary $ mkLine x) xs
  where
    fn :: OverlayList M.ScNote -> (Duration, [M.Element]) -> OverlayList M.ScNote
    fn acc z      = M.addOverlay 0 (mkLine z) acc 

    mkLine :: (Duration, [M.Element]) -> [M.Element]
    mkLine (0,ls) = ls
    mkLine (d,ls) = M.space d : ls




linearPart :: Key -> MetricalSpec -> OverlayList M.ScNote -> M.Part
linearPart k mspec ovs = M.part [M.phrase (M.motif k mspec ovs)]


-------------------------------------------------------------------------------
-- skeletons

-- makeOverlays $ alphLinesToElemLines $ instrLines t smap mus
-- findInst :: H.IName -> H.Music -> Maybe (TranspDist, H.Music)

data Monad m => MotifSkeleton m = MotifSkeleton {
        key_sig      :: Key,
        spell_map    :: SpellingMap,
        meter_spec   :: MetricalSpec,
        mrun         :: m [AlphElem] -> [AlphElem],
        rwrules      :: [RuleTP AlphElem m]
      }

defaultMotifSkeleton :: Key -> MetricalSpec -> MotifSkeleton Identity
defaultMotifSkeleton k mspec = MotifSkeleton {
      key_sig       = k,
      spell_map     = maybe failKey id $ makeSpellingMap k [] ,
      meter_spec    = mspec,
      mrun          = runIdentity,
      rwrules       = [idOne]
    } 
  where
    failKey = error $ "could not make SpellingMap for " ++ show k  



motifSkel :: Monad m => H.IName -> MotifSkeleton m -> H.Music -> Maybe M.Motif
motifSkel name skel skore = step <$> findInst name skore 
  where
    step = mkMotif . mkOvlays . rwLines . uncurry  mkLines
    
    mkLines transn mus  = instrLines transn (spell_map skel) mus
    rwLines ls          = foldl' rwStep ls (rwrules skel) 
    rwStep ls fn        = rewriteLines (mrun skel) fn ls
    mkOvlays ls         = makeOverlays $ alphLinesToElemLines ls  
    mkMotif ovs         = M.motif (key_sig skel) (meter_spec skel) ovs

--------------------------------------------------------------------------------
-- helpers 

renderDocEighty :: PP.Doc -> String
renderDocEighty = (PP.displayS `flip` []) . PP.renderPretty 0.8 80


singleMotifPart :: M.Motif -> M.Part
singleMotifPart m = M.part [M.phrase m]


-- constructors for Alphabet ScNote
anote :: M.Pitch -> [M.NoteAttribute] -> Duration -> AlphElem
anote p xs d = N (M.ScNote p xs) d

arest :: Duration -> AlphElem
arest d = R d

aspacer :: Duration -> AlphElem
aspacer d = S d


-- Haskore - middle_c is (C,5)
-- Mullein - middle_c is c5
                        
cPitch :: H.Pitch -> M.Pitch
cPitch (H.Cf, o)    = M.Pitch M.C M.Flat o
cPitch (H.C,  o)    = M.Pitch M.C M.Nat o
cPitch (H.Cs, o)    = M.Pitch M.C M.Sharp o
cPitch (H.Df, o)    = M.Pitch M.D M.Flat o
cPitch (H.D,  o)    = M.Pitch M.D M.Nat o
cPitch (H.Ds, o)    = M.Pitch M.D M.Sharp o
cPitch (H.Ef, o)    = M.Pitch M.E M.Flat o
cPitch (H.E,  o)    = M.Pitch M.E M.Nat o
cPitch (H.Es, o)    = M.Pitch M.E M.Sharp o
cPitch (H.Ff, o)    = M.Pitch M.F M.Flat o
cPitch (H.F,  o)    = M.Pitch M.F M.Nat o
cPitch (H.Fs, o)    = M.Pitch M.F M.Sharp o
cPitch (H.Gf, o)    = M.Pitch M.G M.Flat o
cPitch (H.G,  o)    = M.Pitch M.G M.Nat o
cPitch (H.Gs, o)    = M.Pitch M.G M.Sharp o
cPitch (H.Af, o)    = M.Pitch M.A M.Flat o
cPitch (H.A,  o)    = M.Pitch M.A M.Nat o
cPitch (H.As, o)    = M.Pitch M.A M.Sharp o
cPitch (H.Bf, o)    = M.Pitch M.B M.Flat o
cPitch (H.B,  o)    = M.Pitch M.B M.Nat o
cPitch (H.Bs, o)    = M.Pitch M.B M.Sharp o


cDur :: H.Dur -> Duration
cDur r = n%d where 
    n = fromIntegral $ numerator r
    d = fromIntegral $ denominator r 

