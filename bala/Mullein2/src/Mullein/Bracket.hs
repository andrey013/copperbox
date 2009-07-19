{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Bracket
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------



module Mullein.Bracket where

import Mullein.Core
import Mullein.Duration


import Data.OneMany

import Data.Ratio


data Step a s = Done | Skip s | Yield a s
  deriving (Eq)

data Binary a = One a | Two a a
  deriving (Eq,Show)


ratDuration :: HasDuration e => e -> Rational
ratDuration = extent . getDuration


apoSkipList :: (st -> a -> Step b st) -> (st -> [a] -> [b]) -> st -> [a] -> [b]
apoSkipList f g st0 xs0 = step st0 xs0 where
  step s []     = g s []
  step s (x:xs) = case f s x of
                    Done       -> g s (x:xs)     -- x must be re-queued
                    Skip s'    -> step s' xs
                    Yield a s' -> a : step s' xs
           

-- | Skipping apomorphism that is unfolded /along/ a list
-- Unfortunately, a single beam step can produce two results:
--   1. the current accumulation 
--   2. a single note too long for a beam group
-- So we have to pollute what could be a general combinator @apoSkipList@ 
-- with the result wrapped in the Binary type.

apoSkipListB :: (st -> a -> Step (Binary b) st) -> (st -> [a] -> [b]) -> st -> [a] -> [b]
apoSkipListB f g st0 xs0 = step st0 xs0 where
  step s []     = g s []
  step s (x:xs) = case f s x of
                    Done               -> g s (x:xs)     -- x must be re-queued
                    Skip s'            -> step s' xs
                    Yield (One a) s'   -> a : step s' xs
                    Yield (Two a b) s' -> a : b : step s' xs



--------------------------------------------------------------------------------
-- bar & beam

bracket :: HasDuration e => MeterPattern -> [e] -> [(Tied,[OneMany e])]
bracket mp notes = map beamer $ bar (sum mp) notes 
  where 
    beamer (h,xs) = (h, beam mp xs)


--------------------------------------------------------------------------------
-- bar

bar :: HasDuration e => Rational -> [e] -> [(Tied,[e])]
bar bar_len ns = apoSkipList (barStep bar_len) barFlush ([],0) ns


barStep :: HasDuration e 
        => Rational -> ([e],Rational) -> e -> Step (Tied,[e]) ([e],Rational)
barStep bar_len (ca,i) note = 
    case compare (i+note_len) bar_len of

      -- fits with room to spare (add to accumulator)
      LT -> Skip (note:ca,i+note_len)
    
      -- exact fit ("close" the bar)
      EQ -> Yield (notTied, reverse $ note:ca) ([],0)

      -- too big (split if possible)
      GT -> case split i note of
              Just (l,r) -> Yield (tied, reverse $ l:ca) (pushback r)

              -- The note doesn't fit but it won't split either
              -- so push it back...
              Nothing    -> Yield (notTied, reverse $ ca) (pushback note)
  where
    pushback a = ([a], ratDuration a)
    note_len   = ratDuration note

split :: HasDuration e => Rational -> e -> Maybe (e,e)
split i note = fn $ splitDuration i $ getDuration note
  where
    fn (Just left, right) = Just (setDuration left note, setDuration right note)
    fn (Nothing,_)        = Nothing

-- 
barFlush :: ([e],Rational) -> [e] -> [(Tied,[e])]
barFlush ([],_) _ = []
barFlush (ca,_) _ = [(notTied, reverse ca)]



--------------------------------------------------------------------------------
-- beam


beam :: HasDuration e => MeterPattern -> [e] ->  [OneMany e]
beam mp notes = apoSkipListB beamStep beamFlush ([],mp) notes


beamFlush :: HasDuration e => ([e],z) -> [e] -> [OneMany e]
beamFlush ([],_)  rs = map one rs
beamFlush (cca,_) rs = fromList (reverse cca) : map one rs

beamStep :: HasDuration e => ([e],[Rational]) 
         -> e
         -> Step (Binary (OneMany e)) ([e],[Rational])
beamStep (ca,stk) e = maybe Done (sk (ratDuration e)) (top stk) where
  sk n d | n >= 1%4 || n > d  = next ca     (Just e) (consume n stk)
         | n == d             = next (e:ca) Nothing  (consume n stk)
         | otherwise          = skip (e:ca) (consume n stk)

  next []  Nothing  ss = Skip ([],ss)
  next []  (Just a) ss = Yield (One (one a)) ([],ss)
  next cca (Just a) ss = Yield (Two (fromList $ reverse cca) (one a)) ([],ss)
  next cca _        ss = Yield (One (fromList $ reverse cca)) ([],ss)

  skip cca ss          = Skip (cca,ss) 




-- Maybe the top of the stack.
top :: [a] -> Maybe a
top (x:_) = Just x
top []    = Nothing


-- reduce the MeterPattern stack by the duration
consume :: (Num a, Ord a) => a -> [a] -> [a]
consume _ []                  = []
consume n (x:xs) | n < x      = (x-n):xs
                 | n == x     = xs
                 | otherwise  = consume (n-x) xs

                      

  
---------------------------------------------------------------------------------
-- overlay
{-

zipOverlays :: [BarP e] -> (BarNum,[BarP e]) -> [BarP e]
zipOverlays bs (bnum,bs') = prefix ++ longZipWith f id id suffix bs' where
    (prefix,suffix)        = splitAt bnum bs
    f (Bar v)        b2    = if null vs then Bar v else Overlay v vs where
                                 vs = voices b2
    f (Overlay v vs) b2    = Overlay v (vs ++ voices b2) 

    voices (Bar v)         = if nullVoice v then [] else [v]
    voices (Overlay v vs)  = v:vs
    
nullVoice :: UnisonP e -> Bool
nullVoice (Unison xs _) = null xs

-}

