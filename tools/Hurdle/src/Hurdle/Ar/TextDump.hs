{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Ar.TextDump
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  
--
-- Pretty print as text...
--
--------------------------------------------------------------------------------

module Hurdle.Ar.TextDump where

import Hurdle.Base.Table
import Hurdle.Ar.Datatypes

import Data.Char
import Text.PrettyPrint.JoinPrint hiding ( length )



printArchive :: ArArchive -> IO ()
printArchive = putStr . archiveText

archiveText :: ArArchive -> String
archiveText = show . ppArchive

ppArchive :: ArArchive -> VDoc
ppArchive a = vconcat $
    [ vdoc    $ text                $ ar_magic a
    , vconcat $ map ppArchiveObject $ ar_objects a
    ]


ppArchiveObject :: ArchiveObject -> VDoc
ppArchiveObject = ppArHeader . ar_header

ppArHeader :: ArHeader -> VDoc
ppArHeader = mkTable "Header" . vcat . sequence fields
  where
    fields = 
       [ recRow 16 "name"               . text . arh_name
       , recRow 12 "date"               . text . arh_date
       , recRow 6  "user id"            . int  . arh_user_id
       , recRow 6  "group id"           . int  . arh_group_id
       , recRow 8  "mode"               . text . arh_mode
       , recRow 10 "size"               . int  . arh_size
       , recRow 2  "trailer"            . tup2 . arh_trailer
       ]
    tup2 (x:y:xs) = oxhex 2 (ord x) <+> oxhex 2 (ord y) <+> text xs
    tup2 xs       = text xs




--------------------------------------------------------------------------------
-- Helpers

-- 4r-24r-24l

colHeadings :: VDoc 
colHeadings = vdoc $ row3 (text "size") (text "field") (text "value") 

row3 :: Doc -> Doc -> Doc -> Doc
row3 = row dblspace (alignRight 4 $ alignRight 24 $ alignLeft 24 $ endrow) 

recRow :: Int -> String -> Doc -> Doc
recRow fsize name doc = row3 (int fsize) (text name) doc

type TableName = String

mkTable :: TableName -> VDoc -> VDoc
mkTable tname rows = title `vcons` (block1 `vappend` rows)
  where
    title  = text tname
    block1 = vconcat [rowSep, colHeadings, rowSep] 


rowSep :: VDoc
rowSep = vdoc $ replicateChar 60 '-' 


-- TO ADD TO JoinPrint
list :: [Doc] -> Doc
list = brackets . punctuate comma

vdoc :: Doc -> VDoc
vdoc = vcat . return

vappend :: VDoc -> VDoc -> VDoc
vappend a b = vconcat [a,b]

