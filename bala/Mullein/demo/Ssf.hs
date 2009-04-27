

module Ssf where

import Mullein.AbcConvert
import Mullein.AbcDoc
import Mullein.AbcOutput
import Mullein.Core
import Mullein.CoreTypes
import Mullein.LabelSet
import Mullein.LilyPondConvert
import qualified Mullein.LilyPondDoc as Ly
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


ssf_abc_part :: PartP ScNote 
ssf_abc_part = convertToAbc lset sn ssf_score where
  lset = maybe (error "lset missing") id $ makeLabelSet e_flat_major

ssf_abc_output :: AbcOutput
ssf_abc_output = 
    generateAbc e_flat_major (fst twoFourTime) (repeat 4) ssf_abc_part


abc_score = tunenum 1 +++ title "Stars and Stripes Forever" 
                      +++ meterinfo (fst twoFourTime)
                      +++ keyinfo a_flat_major
                      +++ abcOutput ssf_abc_output

main = putDoc $ unP $ abc_score

-- Alternative, LilyPond...

ssf_ly_part :: PartP ScNote
ssf_ly_part = convertToLy c4 ssf_score


ssf_ly_output :: LilyPondOutput
ssf_ly_output = generateLilyPond e_flat_major (fst twoFourTime) ssf_ly_part


ly_score = header +++ body where
    header = Ly.header [Ly.title "Stars and Stripes Forever"]
    body   = Ly.book [Ly.score (Ly.lilypondOutput ssf_ly_output)]

lyMain = putDoc $ unP $ ly_score


---


