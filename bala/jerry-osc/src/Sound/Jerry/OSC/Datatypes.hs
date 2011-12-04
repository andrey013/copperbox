{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Jerry.OSC.Datatypes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (at least generalized newtype deriving)
--
-- Concrete syntax tree for MIDI files.
--
-- Values are sometimes not interpreted. This means that the
-- the data types do not fully represent the sematics of the 
-- data, but all the data is either stored in the tree or 
-- synthesizeable.
-- 
-- @ readFile >>= writeFile @ will produce an identical binary \[1\]. 
--
-- \[1\] Or it should, failure indicates a bug...
--
--------------------------------------------------------------------------------


module Sound.Jerry.OSC.Datatypes 
  (

    Packet(..)
  , Atom(..)

  , typeTag
    
  ) where


import qualified Data.ByteString.Lazy as L


data Packet = Message { msg_address :: String, arguments :: [Atom] }
            | Bundle 
  deriving (Eq,Ord,Show)


data Atom = Int32 Int
          | TimeTag 
          | Float32 Float
          | String String
          | Blob L.ByteString
  deriving (Eq,Ord,Show)



typeTag :: Atom -> Char
typeTag (Int32 {})      = 'i'
typeTag TimeTag         = 't'
typeTag (Float32 {})    = 'f'
typeTag (String {})     = 's'
typeTag (Blob {})       = 'b'
