{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Hawa.Dummy
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Vector drawing... 
--
--------------------------------------------------------------------------------

module Graphics.Hawa.Dummy where

import Graphics.Hawa.Unit

import qualified Data.Foldable as F
import Data.Sequence ( (><), (|>), ViewL(..) )
import qualified Data.Sequence as S
import Prelude hiding ( cycle )


import Text.PrettyPrint.Leijen

type Point = (Unit,Unit)




newtype Path = Path { getPath :: S.Seq PathSegement }
  deriving (Eq,Show)
  
  
type Line = Point -> Point -> Path

line :: Point -> Point -> Path 
line a b = Path $ S.singleton (MoveTo a) |>  (LineTo b) 

curve :: Point -> Point -> Point -> Path 
curve a b c = Path $ S.singleton (MoveTo a) |>  (CurveTo a b c) 

draw :: [Path] -> PictureC
draw = Seq . S.fromList . map draw1 where
    draw1 p = Seq $ S.singleton (PathC p) |> Stroke

cycle :: Path -> Path
cycle = Path . (|> Closepath) . getPath

-- concat paths -- if p1.last /= p2.first then there will be a visible 
-- gap in the path
(&) :: Path -> Path -> Path 
(&) p1 p2 = Path $ (getPath p1) >< (getPath p2)   

data PathSegement = LineTo Point
                  | MoveTo Point
                  | CurveTo Point Point Point
                  | Fill 
                  | Closepath
  deriving (Eq,Show)
  
  
data PictureC = PathC Path
              | Stroke
              | Seq (S.Seq PictureC)
  deriving (Eq,Show)

type SrcCode = String

run :: FilePath -> PictureC -> IO ()
run path p = writeFile path (show $ pp p)

class PP a where pp :: a -> Doc


instance PP PictureC where
  pp (PathC p)            = pp p
  pp Stroke               = text "stroke"
  pp (Seq se)             = vcats se 

instance PP Path where
  pp (Path se)            = vcats se

instance PP PathSegement where
  pp (LineTo pt)          = ppPoint pt <+> text "lineto"  
  pp (MoveTo pt)          = ppPoint pt <+> text "moveto"
  pp Fill                 = text "fill"
  pp (CurveTo p1 p2 p3)   = ppPoint p1 <+> ppPoint p2 <+> ppPoint p3 <+> text "curveto"
  pp Closepath            = text "closepath"

ppPoint :: Point -> Doc
ppPoint (a,b)
    | whole a && whole b  = asWhole a <+> asWhole b
    | otherwise           = asDouble a <+> asDouble b
  where
    asWhole   = int . fst . properFraction
    asDouble  = double . getUnit
             
vcats :: PP a => S.Seq a -> Doc
vcats = step . S.viewl where
    step EmptyL         = empty      
    step (a :< sa)      = F.foldl' (\ac e -> ac <$> pp e) (pp a) sa  