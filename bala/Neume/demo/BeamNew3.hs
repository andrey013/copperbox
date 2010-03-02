{-# LANGUAGE TypeFamilies               #-}


module BeamNew3 where

import Neume.Bracket
import Neume.Datatypes
import Neume.Duration
import Neume.Doc
import Neume.NamedElements
import Neume.SyntaxStaff ( StdGlyph )

import Text.PrettyPrint.Leijen hiding ( dot, empty )
import qualified Text.PrettyPrint.Leijen as Doc

import Data.Foldable ( toList )
import Data.Sequence
import Prelude hiding ( null, length )


docEmpty :: Doc
docEmpty = Doc.empty


newtype Notes a = Notes { getNotes :: [BeamTree a]}
  deriving (Show)

data BeamTree a = Beam           (Notes a)
                | N_Plet Int Int (Notes a)
                | S1 a
  deriving (Show)

x1e :: StdGlyph
x1e = c 5 () en

x1qd :: StdGlyph
x1qd = c 5 () (dot qn) 


read_192 :: NoteList StdGlyph
read_192 = NoteList $ [plet5 , S x1qd] where
  plet5 = plet 5 3 [S x1e, S x1e, S x1e, S x1e, S x1e]


plet5 :: PletTree StdGlyph
plet5 = Plet 5 3 (NoteList $ [S x1e, S x1e, S x1e, S x1e, S x1e])


demo1 = renderDocEighty $ pretty read_192

demo2 = renderDocEighty $ pretty $ beam read_192

beam :: (Measurement a ~ DurationMeasure, NumMeasured a) 
     => NoteList a -> Notes a
beam = Notes . beamNotes . getNoteList 

beamNotes :: (Measurement a ~ DurationMeasure, NumMeasured a) 
     => [PletTree a] -> [BeamTree a]
beamNotes = alternateUnwind out inn unbuffer
  where
    out a@(S e) 
       | umeasure a < quarter_note && rendersToNote e = Nothing
    out a@(Plet _ _ _) 
       | pletAll ((<quarter_note) . nmeasure) a       = Nothing
    out pt                                            = Just (conv pt)
    
       
    inn a | pletAll ((<quarter_note) . nmeasure) a    = Just (conv a)
          | otherwise                                 = Nothing


conv :: PletTree a -> BeamTree a
conv (S a)            = S1 a
conv (Plet p q notes) = N_Plet p q (Notes $ map conv $ getNoteList notes)

rendersToNote :: a -> Bool
rendersToNote _ = True

pletAll :: (a -> Bool) -> PletTree a -> Bool
pletAll test (S a)            = test a
pletAll test (Plet _ _ notes) = step (getNoteList notes) where
   step []                      = True
   step (p:ps) | pletAll test p = step ps
   step _                       = False


rendersToRest = not . rendersToNote

alternateUnwind :: (a -> Maybe b) 
                -> (a -> Maybe interim) 
                -> (Seq interim -> [b]) 
                -> [a] -> [b]
alternateUnwind outside inside flush_buffer inp = outstep inp 
  where
    outstep []        = []
    outstep (x:xs)    = case outside x of
                         Just a -> a : outstep xs
                         Nothing  -> instep empty (x:xs)
    
    instep buf []     = flush_buffer buf
    instep buf (x:xs) = case inside x of
                          Just a -> instep (buf |> a) xs
                          Nothing -> flush_buffer buf ++ outstep (x:xs)


unbuffer :: Seq (BeamTree a) -> [BeamTree a]
unbuffer = step [] . viewr 
  where
    step acc EmptyR     = acc
    step acc (se :> S1 a)  
        | rendersToNote a && null se = S1 a : acc
        | rendersToNote a            = (beam $ toList $ se |> S1 a) : acc
        | otherwise                  = step (S1 a : acc) (viewr se)  

    -- if the right edge is a n_plet always beam
    step acc (se :> bt)              = (beam $ toList $ se |> bt) : acc

    beam = Beam . Notes

--------------------------------------------------------------------------------
-- Pretty instances

ppBT :: Pretty a => (Doc -> Doc -> Doc) -> BeamTree a -> Doc 
ppBT _  (S1 a)             = pretty a
ppBT op (N_Plet p q notes) = braces (int p <> colon <> int q <+> ppList op notes)
ppBT _  (Beam notes)       = ppList uline notes

uline :: Doc -> Doc -> Doc
uline a b = a <> char '_' <> b 


ppList :: Pretty a => (Doc -> Doc -> Doc) -> Notes a -> Doc
ppList op (Notes xs) = step xs where
  step []     = docEmpty
  step [x]    = ppBT op x
  step (x:xs) = (ppBT op x) `op` step xs



instance Pretty a => Pretty (Notes a) where
  pretty = ppList (<+>)



