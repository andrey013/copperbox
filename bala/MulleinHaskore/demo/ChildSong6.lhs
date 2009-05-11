\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim} 

> module ChildSong6 where
> import Haskore
>
> import MulleinHaskore.LilyPond
> import MulleinHaskore.Translate
>
> import Mullein.Rewriting
> import Mullein.StringRewriting
> import qualified Mullein.Core          as M
> import qualified Mullein.NamedElements as M
> import qualified Mullein.Rewriting     as M
> import qualified Mullein.SpellingMap   as M
>
> import Control.Applicative
> import Data.Ratio
> 
> -- note updaters for mappings
> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> lmap f l = line (map f l)
> 
> -- repeat something n times
> times  1    m = m
> times (n+1) m = m :+: (times n m)
> 
> -- Baseline:
> b1 = lmap (fd dqn) [b  3, fs 4, g  4, fs 4]
> b2 = lmap (fd dqn) [b  3, es 4, fs 4, es 4]
> b3 = lmap (fd dqn) [as 3, fs 4, g  4, fs 4]
> 
> bassLine = times 3 b1 :+: times 2 b2 :+: times 4 b3 :+: times 5 b1
> 
> -- Main Voice:
> v1  = v1a :+: v1b
> v1a = lmap (fd en) [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
> v1b = lmap vol    [cs 5 tn, d 5 (qn-tn), cs 5 en, b 4 en]
> 
> v2  = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f
> v2a = lmap vol [cs 5 (dhn+dhn), d 5 dhn, 
>                 f 5 hn, gs 5 qn, fs 5 (hn+en), g 5 en]
> v2b = lmap (fd en) [fs 5, e 5, cs 5, as 4] :+: a 4 dqn v :+:
>       lmap (fd en) [as 4, cs 5, fs 5, e 5, fs 5, g 5, as 5]
> v2c = lmap vol [cs 6 (hn+en), d 6 en, cs 6 en, e 5 en] :+: enr :+: 
>       lmap vol [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en]
> v2d = lmap (fd en) [fs 5, cs 5, e 5, cs 5, a 4, as 4, d 5, e 5, fs 5] :+:
>       lmap vol [fs 5 tn, e 5 (qn-tn), d 5 en, e 5 tn, d 5 (qn-tn),
>                 cs 5 en, d 5 tn, cs 5 (qn-tn), b 4 (en+hn)]
> v2e = lmap vol [cs 5 en, b 4 en, fs 5 en, a 5 en, b 5 (hn+qn), a 5 en,
>                 fs 5 en, e 5 qn, d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn]
> v2f = Tempo (3/2) (lmap vol [cs 5 en, d 5 en, cs 5 en]) :+: b 4 (3*dhn+hn) v
> 
> mainVoice = times 3 v1 :+: v2
> 
> -- Putting it all together:
> childSong6 = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine :=: mainVoice))


Note - (hn+en) cannot be printed by Mullein.
Maybe the mertical splitting code should see this as something to be 
split in two and tied.


>
> -- helping Mullein:
> -- avoid as much parallelism as possible.
>
> childSong6Bass = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine))
> childSong6Main = Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))
>

> 
> cs6_ly = writeFile "childsong6.ly" 
>                    $ renderDocEighty 
>                    $ singleMelodyScoreSkel lySkel
>                    $ singleMotifPart 
>                    $ maybe failK id
>                    $ motifSkel "piano" mSkel childSong6Main
>   where 
>     mSkel     = (defaultMotifSkeleton cs6_key cs6_mtr) { rwrules = cs6_rules }
>     cs6_rules = [ elimHnEn, graceTnQn, elimDhnDhn ]
>     lySkel    = defaultSingleMelodyScoreSkeleton "Children's Song No.6" 
>                                                  cs6_key 
>                                                  cs6_mtr
> 
>     failK     = error "Could not find/render piano part"
>
> cs6_key   = M.d_major
> cs6_mtr   = M.withMeterPattern [3%8,3%8] $ M.metricalSpec 6 8 



Ideally elimHnEn should tie the two notes is produces from
one (hn+en) note. This doesn't happen as there ties are
not at the 'element' level in the syntax tree.


>
> elimHnEn :: RuleTP AlphElem m
> elimHnEn = preserving $ 
>     (\(N p _) -> listD [N p (1%2), N p (1%8)]) <$> matchesDur (5%8) note
>
> -- grace where tn preceeds a (qn-tn)
> graceTnQn :: RuleTP AlphElem m 
> graceTnQn =  preserving $
>     (\(N p _) (N p' _) -> listD [M.G p (1%32), N p' (1%4)]) 
>         <$> matchesDur (1%32) note <*> matchesDur ((1%4)-(1%32)) note
>
>
> elimDhnDhn :: RuleTP AlphElem m
> elimDhnDhn = preserving $ 
>     (\(N p _) -> listD [N p (3%4), N p (3%4)]) <$> matchesDur (3%2) note
> 

>
> cs6B_ly = writeFile "childsong6Bass.ly" 
>                    $ renderDocEighty 
>                    $ singleMelodyScoreSkel lySkel
>                    $ singleMotifPart 
>                    $ maybe failK id
>                    $ motifSkel "piano" mSkel childSong6Bass
>   where 
>     mSkel     = (defaultMotifSkeleton cs6_key cs6_mtr) { rwrules = cs6_rules }
>     cs6_rules = [ elimHnEn, graceTnQn, elimDhnDhn ]
>     lySkel    = defaultSingleMelodyScoreSkeleton "Children's Song No.6" 
>                                                  cs6_key 
>                                                  cs6_mtr
> 
>     failK     = error "Could not find/render piano part"
>

\end{verbatim} }
