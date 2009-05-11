{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.OutputCommon
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common code for LilyPond and ABC output (e.g. same Monad)
--
--------------------------------------------------------------------------------


module Mullein.OutputCommon where

import Mullein.Core
import Mullein.Duration

import Control.Monad.State
import Data.Sequence ( (><), (<|), (|>), ViewL(..), ViewR(..) )
import qualified Data.Sequence as S
import Text.PrettyPrint.Leijen


data OutputFragment = MidtuneCmd Doc 
                    | BarOutput Doc
                    | RepStart        -- "|:"
                    | RepEnd          -- ":|"
                    | RepEnding Int   -- ":[1" 
                    | SglBar
                    | DblBar
  deriving (Show)


data St  = St { current_key :: Key, current_meter :: Meter }

type M a = State St a



keyChange :: Key -> (Key -> Doc) -> M (Maybe Doc)
keyChange new pp = do 
    old <- gets current_key 
    if (new==old) 
       then return Nothing
       else do {modify $ \s -> s{current_key=new} ; return $ Just $ pp new } 



meterChange :: Meter -> (Meter -> Doc) -> M (Maybe Doc)
meterChange new pp = do 
    old <- gets current_meter
    if (new==old) 
       then return Nothing
       else do { modify $ \s -> s{current_meter=new} 
               ; return $ Just $ pp new } 


--------------------------------------------------------------------------------
-- output fragments

appendBarFragment :: S.Seq OutputFragment -> Doc -> S.Seq OutputFragment
appendBarFragment se x = se S.|> (BarOutput x)

motifFragment :: Maybe Doc -> Maybe Doc -> [Doc] -> S.Seq OutputFragment
motifFragment opt_key opt_meter xs = 
    mbAdd opt_key $ mbAdd opt_meter $ S.fromList $ map BarOutput xs
  where
    mbAdd a se = maybe se (\x -> (MidtuneCmd x) S.<| se) a



repeated :: S.Seq OutputFragment -> S.Seq OutputFragment
repeated se = RepStart <| (se |> RepEnd) 

fsrepeat :: S.Seq OutputFragment
         -> S.Seq OutputFragment
         -> S.Seq OutputFragment
         -> S.Seq OutputFragment
fsrepeat se x y = RepStart <| se >< end where
    end = (RepEnding 1)  <| x >< ((RepEnding 2) <| (y |> RepEnd)) 


-- If a tune starts with a repeated, the repeat start sign '|:', doesn't
-- need to be printed
dropRepStart :: S.Seq OutputFragment -> S.Seq OutputFragment
dropRepStart s0 = step $ S.viewl s0 where
    step (RepStart :< se)   = se
    step _                  = s0

addDblEnd :: S.Seq OutputFragment -> S.Seq OutputFragment
addDblEnd s0 = step $ S.viewr s0 where
   step (_ :> BarOutput _)  = s0 |> DblBar
   step _                   = s0 

intersperseBars :: [OutputFragment] -> [OutputFragment]
intersperseBars (BarOutput d1 : BarOutput d2 : xs) = 
    BarOutput d1 : SglBar : intersperseBars (BarOutput d2 : xs)
intersperseBars (x:xs)      = x : intersperseBars xs
intersperseBars []          = []
