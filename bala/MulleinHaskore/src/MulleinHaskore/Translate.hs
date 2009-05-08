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

import Mullein.Duration
import Mullein.Rewriting

import qualified Mullein.Score       as M

import qualified Haskore.Basics      as H

type OnsetTime = Duration
type AlphElem = Alphabet M.ScNote

type Line  = (OnsetTime, [AlphElem])


-- Note - extract just the 'music' not the onset
-- music may have been transposed...


findInst :: H.IName -> H.Music -> Maybe (Int, H.Music)
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


{-

untree :: Duration -> H.Music -> TransM [Line]
untree start skore = step start (start,S.empty) [] skore
  where
    step :: Duration -> InstLine -> [InstLine] -> H.Music -> TransM (Duration, [InstLine])
    step t z zs (H.Note p d xs)     = (\note -> (t + cDur d, z `snoc` note:zs)) 
                                        <$> mkANote p xs d

    step t z zs (H.Rest d)          = pure (t+d', z `snoc` (arest d'):zs)
                                      where d' = cDur d


    step t z zs (lhs H.:+: rhs)     = do { (t',z':zs') <- step t z zs lhs
                                         ; step t' z' zs' rhs }


    step t z zs (lhs H.:=: rhs)     = do { (_,zs') <- untree t instr rhs
                                         ; step t z (zs++zs') lhs }

    step t z zs (H.Tempo _ mus)     = step t z zs mus

    step t z zs (H.Trans i mus)     = do { j <- asks transp
                                         ; local (\e -> e {transp=j+i}) 
                                                 (step t z zs mus) }

    step t z zs (H.Instr nom mus)   = undefined

    step t z zs (H.Player _ mus)    = step t z zs mus
    
    step t z zs (H.Phrase _ mus)    = step t z zs mus

    snoc (name,t,se) e = (name,t,se |> e)
    
    add (d,nm,se) xs  | S.null se = xs
                      | otherwise = (d,nm,se):xs

-}
