{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ZParse.SrcPos
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SrcPos - ...
--
--------------------------------------------------------------------------------

module Text.ZParse.SrcPos where


data StreamPosn = StreamPosn { char_number :: !Int }
  deriving (Eq,Show)

data TextPosn = TextPosn { line_number :: !Int, column_number :: !Int }
  deriving (Eq,Show)

class SrcPos a where
  initialPos :: a
  nextPos    :: Char -> a -> a
  furthest   :: a -> a -> a
  
instance SrcPos StreamPosn where
  initialPos = StreamPosn 1
  nextPos _ (StreamPosn i) = StreamPosn $ i+1
  furthest a@(StreamPosn i) b@(StreamPosn j) = if j > i then b else a
  
instance SrcPos TextPosn where
  initialPos = TextPosn 1 1
  nextPos '\n' (TextPosn l _) = TextPosn (l+1) 1
  nextPos '\t' (TextPosn l c) = TextPosn l (incr c) where
      incr i = (i + 8 - ((i-1) `mod` 8))
  nextPos _    (TextPosn l c) = TextPosn l (c+1)
  furthest i@(TextPosn l c) j@(TextPosn l' c') 
                              = if (l',c') > (l,c) then j else i



 
   