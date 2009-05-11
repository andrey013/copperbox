The first phrase of the flute part of "Stars and Stripes Forever."

\begin{verbatim}

> module Ssf where
> import Haskore
> import MulleinHaskore.LilyPond
> import MulleinHaskore.Translate
>
> import Mullein.Rewriting
> import Mullein.StringRewriting
> import qualified Mullein.Core          as M
> import qualified Mullein.NamedElements as M
> import qualified Mullein.SpellingMap   as M
>
> import Control.Applicative
> import Data.Ratio
>
> ssfMelody = line (m1 ++ m2 ++ m3 ++ m4)
> m1 = [		    trilln 2 5 (bf 6 en []), 
> 	Phrase [Art staccato]    (line [ef 7 en [],
>			   		ef 6 en [],
>			   		ef 7 en []])]
>
> m2 = [Phrase [Art legato]      (line [bf 6 sn [],
>			   		c  7 sn [],
>			   		bf 6 sn [],
>			   		g  6 sn []]),
>	Phrase [Art staccato]    (line [ef 6 en [],
>			   		bf 5 en []])]
>
> m3 = [Phrase [Art legato]       (line [ef 6 sn [],
>			   		f  6 sn [],
>			   		g  6 sn [],
>			   		af 6 sn []]),
>	Phrase [Art staccato]    (line [bf 6 en [],
>			   		ef 7 en []])]
>
> m4 = [		    trill 2 tn (bf 6 qn []), 
>					bf 6 sn [],
>					denr]
>
> ssf = Instr "flute" (Tempo 2 (ssfMelody))
>
>
> ssf_ly = writeFile "ssf.ly" $ renderDocEighty 
>                             $ singleMelodyScoreSkel lySkel
>                             $ singleMotifPart 
>                             $ maybe failK id
>                             $ motifSkel "flute" mSkel ssf 
>   where 
>     mSkel     = (defaultMotifSkeleton ssf_key ssf_mtr) { rwrules = elim_ts }
>     elim_ts   = [elimTrill40, elimTrill32]
>     lySkel    = defaultSingleMelodyScoreSkeleton "Stars and Stripes Forever" 
>                                                  ssf_key 
>                                                  ssf_mtr
>     ssf_key   = M.e_flat_major
>     ssf_mtr   = M.metricalSpec 2 4
> 
>     failK     = error "Could not find/render flute part"
>


>
> elimTrill40 :: RuleTP AlphElem m
> elimTrill40 = preserving $ 
>     (\(N p _:_) -> wrapD (N p (1%8))) <$> count 5 (matchesDur (1%40) note)
>
> elimTrill32 :: RuleTP AlphElem m
> elimTrill32 = preserving $ 
>     (\(N p _:_) -> wrapD (N p (1%4))) <$> count 8 (matchesDur (1%32) note)
>

Debugging 

>
> ssf_ly0 = error $ show 
>                             $ maybe failK id
>                             $ motifSkel "flute" mSkel ssf 
>   where 
>     mSkel     = (defaultMotifSkeleton ssf_key ssf_mtr) { rwrules = elim_ts }
>     elim_ts   = [elimTrill40, elimTrill32]
>     lySkel    = defaultSingleMelodyScoreSkeleton "Stars and Stripes Forever" 
>                                                  ssf_key 
>                                                  ssf_mtr
>     ssf_key   = M.e_flat_major
>     ssf_mtr   = M.metricalSpec 2 4
> 
>     failK     = error "Could not find/render flute part"
>
