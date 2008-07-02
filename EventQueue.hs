--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.OnsetQueue
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  portable
--
-- A queue with elements tagged by onset time. 
-- Principally for generating MIDI files when splitting \notes\ 
-- into @note-on@ and @note-off@. The note-off will occur at some
-- point in the future so it is queued to be realized later.   
--
--------------------------------------------------------------------------------


module Bala.Perform.OnsetQueue (
  -- * Onset queue (opaque)
  OnsetQueue, 
  -- * Operations
  empty, add, upto, firstOnset, rest
  ) where


import Data.Set hiding (empty, map)
import qualified Data.Set as Set
import Prelude hiding (length, null)



type Elt a = Pair Integer a

data OnsetQueue a = OnsetQueue {
    -- | Cache the time of the first onset for quick look up
    first_onset :: Integer,
    -- | Keep elements in a (ordered) set. 
    elements    :: Set (Elt a)    
  }



instance Show a => Show (OnsetQueue a) where
  showsPrec p (OnsetQueue _ s) = showsPrec p s 

-- 'Ord' should only operate on the onset (the fst element), 
-- this rules out using the standard pair (,) which looks at
-- the second element if the first is equal.
data Pair a b = Pair a b

instance (Show a, Show b) => Show (Pair a b) where
  showsPrec p (Pair a b) = showsPrec p (a,b)


instance Eq a => Eq (Pair a b) where
  (==) (Pair a _) (Pair a' _) = a == a'
  
instance Ord a => Ord (Pair a b) where
  compare (Pair a _) (Pair a' _) = a `compare` a'

-- | The empty queue.
empty :: OnsetQueue a
empty = OnsetQueue (-1) Set.empty

-- | Insert an element @a@ into the queue @q@ at onset time @o@.
--   
-- > add o a q 
--
add :: Integer -> a -> OnsetQueue a -> OnsetQueue a
add o a (OnsetQueue fo s) 
    | fo < 0 || o < fo    = OnsetQueue o (insert (Pair o a) s) 
    | otherwise           = OnsetQueue fo (insert (Pair o a) s)

firstOnset :: OnsetQueue a -> Integer
firstOnset (OnsetQueue fo _) = fo

-- | Split the onset queue into an ordered list of elements occuring before 
-- the onset and a queue of those remaining.
upto :: Integer -> OnsetQueue a -> ([(Integer,a)], OnsetQueue a)
upto i q 
    | i < firstOnset q  = ([],q)
    | otherwise         = let (a,b) = leftright i q in (restSet a, b)
  where
    

leftright :: Integer -> OnsetQueue a -> (Set (Elt a),OnsetQueue a)
leftright i (OnsetQueue _ s) = 
    let (a,b) = partition ((>) (Pair i undefined)) s
    in (a, OnsetQueue (firstOf b) b)
  where 
    firstOf s | null s   = (-1)
              | otherwise = let (Pair i _) = findMin s in i  
 
-- | Take the remaining elements of the queue as a ordered list.
-- (For MIDI you would want to do this after you have split the last pending 
-- \note\ into a @note-on@ and @note-off@ and you need to account for the 
-- remaining note-ffs.  
rest :: OnsetQueue a -> [(Integer,a)]
rest (OnsetQueue _ s) = restSet s

restSet :: Set (Elt a) -> [(Integer,a)]
restSet = fold fn []
  where
    fn (Pair a b) acc = (a,b) : acc
    
