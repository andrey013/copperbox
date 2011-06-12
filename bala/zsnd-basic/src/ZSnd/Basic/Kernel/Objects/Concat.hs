{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.Concat
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Classes for concatenation.
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.Concat
  (


    Overlay(..)
  , overlays

  , Append(..)
  , appends

  , Space(..)
  , spaces
  
  ) where

import ZSnd.Basic.Kernel.Base.BaseDefs


import Data.Monoid

infixr 6 `overlay`

-- | Overlay one event over another - start points
-- coincide.
--
-- > `overlay` (infixr 6)
--
class Overlay o where
  overlay :: o -> o -> o 


overlays :: (Monoid o, Overlay o) => [o] -> o
overlays []     = mempty
overlays (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (a:as) = go (acc `overlay` a) as
  



infixr 6 `append`


-- | Concatenation with /displacement/ - the second object is 
-- moved so that it starts as the first finishes.
--
-- > `append` (infixr 6)
-- 
class Append o where
  append :: o -> o -> o
  



-- | Linear concatenate a list of objects.
-- 
-- Note - the first argument is an /alternative/ - this is drawn 
-- if the list is empty, otherwise it is not drawn.
--
appends :: (Monoid o, Append o) => [o] -> o
appends []     = mempty
appends (x:xs) = go x xs
  where
    go acc []     = acc
    go acc (a:as) = go (acc `append` a) as
  


  

class Space o where
   space :: u ~ DUnit o => u -> o -> o -> o

spaces :: (Monoid o, Space o, u ~ DUnit o) => u -> [o] -> o
spaces _  []     = mempty
spaces dx (x:xs) = go x xs
  where
    op            = space dx
    go acc []     = acc
    go acc (a:as) = go (acc `op` a) as
  

--
-- No analogue to alignment in Wumpus, as we only in the temporal 
-- domain.
-- 
-- However there is probably some need to align events at end time
-- rather than start time. 
--

