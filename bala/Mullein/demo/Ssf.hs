

module Ssf where

import Mullein.AbcConvert
import Mullein.AbcOutput
import Mullein.Core
import Mullein.CoreTypes
import Mullein.LabelSet
import Mullein.LilyPondConvert
import Mullein.LilyPondOutput
import Mullein.NamedElements
import Mullein.Score

import Text.PrettyPrint.Leijen ( putDoc )

type Line = OverlayList ScNote

line :: [ElementP ScNote] -> OverlayList ScNote
line = primary 



m1 :: Line
m1 = line [ {- trill -} bf 6 sn [], ef 7 en [], ef 6 en [], ef 7 en [] ]

m2 :: Line
m2 = line 
  [ {- legato -}   bf 6 sn [], c 7 sn [], bf 6 sn [], g  6 sn [],
    {- staccato -} ef 6 en [], bf 5 en []]

m3 = line 
  [ {- legato -}   ef 6 sn [], f  6 sn [], g  6 sn [], af 6 sn [],
    {- staccato -} bf 6 en [], ef 7 en []]

m4 = line [ {- trill -} bf 6 qn [], bf 6 sn [], denr]


mkMotif = motif e_flat_major twoFourTime

twoFourTime = metricalSpec 2 4

ssf_score = part $ map (phrase .  mkMotif) [m1,m2,m3,m4]


ssf_abc :: PartP ScNote 
ssf_abc = convertToAbc lset sn ssf_score where
  lset = maybe (error "lset missing") id $ makeLabelSet e_flat_major

ly = putDoc $ outputLy e_flat_major (fst twoFourTime) ssf_ly where
  ssf_ly = convertToLy c4 ssf_score


main = putDoc $ outputAbc e_flat_major (fst twoFourTime) (repeat 4) ssf_abc


