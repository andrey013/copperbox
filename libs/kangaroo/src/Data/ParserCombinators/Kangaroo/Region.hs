{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.Region
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Data types to manage parse regions
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.Region
  (
  -- * Types
    RegionCoda(..)
  , RegionStart
  , RegionEnd
  , RegionName
  , Pos
  , RegionInfo
  , ParseStack
  , RegionError(..)

  -- * Operations
  , newStack
  , newRegion

  , regionStart
  , regionEnd
  , push
  , pop
  , move1
  , move
  , location

  , printParseStack
  
  ) where


import Text.PrettyPrint.JoinPrint  hiding ( length )


-- | 'RegionCoda' - three useful final positions:
--
-- 1. dalpunto  - 'from the point'      
-- - Run the parser within a region and return to where you came
--   from.
--
-- 2. alfermata - 'to the stop'    
-- - Run the parser within a region, the cursor remains wherever 
--   the parse finished.
--
-- 3. alfine    - 'to the end'     
-- - Run the parser within a region and jump to the right-end of 
--   the region after the parse.
--
data RegionCoda = Dalpunto | Alfermata | Alfine
  deriving (Enum,Eq,Show)


type RegionStart = Int
type RegionEnd   = Int
type RegionName  = String

type Pos         = Int

-- | 'RegionInfo' contains the (inclusive) bounds of a region 
-- and the \'coda action\' to take after parsing has finished.
--

data RegionInfo  = RegionInfo 
        { region_start_incl   :: !RegionStart
        , region_end_incl     :: !RegionEnd
        , region_coda         :: !RegionCoda
        , region_name         :: !String
        }
  deriving (Eq,Show)



-- | 'ParseStack' is a non empty list of RegionInfo structures.
--
data ParseStack = P0 Pos RegionInfo | Pn Pos RegionInfo ParseStack
  deriving (Eq,Show)

-- These two might change...

newtype RegionError = RegionError { getRegionError :: String }
  deriving (Eq,Show)



--------------------------------------------------------------------------------

mapTop :: (RegionInfo -> a) -> ParseStack -> a
mapTop f (P0 _ info)   = f info
mapTop f (Pn _ info _) = f info

modifyPos :: (Pos -> Pos) -> ParseStack -> ParseStack
modifyPos f (P0 p info)     = P0 (f p) info
modifyPos f (Pn p info stk) = Pn (f p) info stk

validBounds :: RegionStart -> RegionEnd -> ParseStack -> Bool
validBounds s e stk = s >= regionStart stk && e <= regionEnd stk

infos :: ParseStack -> [RegionInfo]
infos (P0 _ info)     = [info]
infos (Pn _ info stk) = info : infos stk

regionError :: RegionName -> RegionStart -> RegionEnd -> ParseStack 
            -> RegionError
regionError nm s e stk = step (regionStart stk) (regionEnd stk) 
  where
    step rs re | s < rs && e > re = mkMsg nm "past the bounds" (s,e) (rs,re)
               | s < rs           = mkMsg nm "past the left"   (s,e) (rs,re)
               | e > re           = mkMsg nm "past the right"  (s,e) (rs,re)
               | otherwise        = RegionError $ 
                                      "regionError called on invalid data."

mkMsg :: RegionName -> String -> (Int,Int) -> (Int,Int) -> RegionError
mkMsg name descr new old  = RegionError $ 
    unwords [ "The new region"
            , ('\'' : name ++ "'")
            , show new
            , "extends"
            , descr
            , "of the old region"
            , show old
            ]

--------------------------------------------------------------------------------
-- Exported operations

regionStart     :: ParseStack -> Int
regionStart     = mapTop region_start_incl

regionEnd       :: ParseStack -> Int
regionEnd       = mapTop region_end_incl


newStack :: RegionStart -> RegionEnd -> RegionCoda -> RegionName -> ParseStack
newStack s e coda name = P0 0 (RegionInfo s e coda name)

newRegion :: RegionStart -> Int -> RegionCoda -> RegionName -> RegionInfo
newRegion s len coda name = RegionInfo s (s+len-1) coda name


push :: RegionInfo -> ParseStack -> Either RegionError ParseStack
push info@(RegionInfo s e _ name) stk 
    | validBounds s e stk = Right $ Pn s info stk
    | otherwise           = Left  $ regionError name s e stk


pop :: ParseStack -> ParseStack
pop (P0 p info)      = P0 p info
pop (Pn p info stk)  = case region_coda info of
    Dalpunto  -> stk
    Alfermata -> modifyPos (const p) stk
    Alfine    -> modifyPos (const $ region_end_incl info) stk

-- Moving will always succeed, so it is possible to move beyond 
-- the end-of-file.
                         
move1 :: ParseStack -> ParseStack
move1 = modifyPos (+1)

move :: (Pos -> Pos) -> ParseStack -> ParseStack
move = modifyPos

location :: ParseStack -> Int
location (P0 p _)   = p
location (Pn p _ _) = p

printParseStack :: ParseStack -> String
printParseStack pstack = render $ vcat $ map fn stk
  where
    stk             = infos pstack
    (w1,w4)         = onSnd (length . show) $ foldr phi (0,0) stk
    phi info (a,b)  = (max a (length $ region_name info), max b $ region_end_incl info)
    onSnd f (a,b)   = (a, f b)

    fn        :: RegionInfo -> Doc
    fn rgn    =  alignPad AlignLeft w1 ' ' (text $ region_name rgn)
             <+> alignPad AlignLeft w4 ' ' (int  $ region_start_incl rgn)
             <+> alignPad AlignLeft w4 ' ' (int  $ region_end_incl rgn)
    

