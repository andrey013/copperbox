\section{Example of Simple Self-Similar (Fractal) Music}
\label{self-similar}

{\small\begin{verbatim} 

> module SelfSim where
>
> import Haskore

An example of self-similar, or fractal, music.

> data Cluster = Cl SNote [Cluster]  -- this is called a Rose tree
> type Pat     = [SNote]
> type SNote   = [(AbsPitch,Dur)]    -- i.e. a chord
>

> chord = foldr1 (:=:)
>
> sim :: Pat -> [Cluster]
> sim pat = map mkCluster pat
>     where mkCluster notes = Cl notes (map (mkCluster . addmult notes) pat)
> 
> addmult pds iss = zipWith addmult' pds iss
>                   where addmult' (p,d) (i,s) = (p+i,d*s)
>
> simFringe n pat = fringe n (Cl [(0,0)] (sim pat))
> 
> fringe 0 (Cl note cls) = [note]
> fringe n (Cl note cls) = concat (map (fringe (n-1)) cls)
> 
> -- this just converts the result to Haskore:
> simToHask s = let mkNote (p,d) = Primitive (Note d (pitch p))
>               in line (map (chord . map mkNote) s)
>
> -- and here are some examples of it being applied:
>
> sim1 n = Modify (Instrument AcousticBass) 
>            (Modify (Transpose 36)
>               (Modify (Tempo 4) (simToHask (simFringe n pat1))))
> t6 = play (sim1 4)
>
> sim2 n = Modify (Instrument AcousticGrandPiano)
>            (Modify (Transpose 53)
>               (Modify (Tempo 4) (simToHask (simFringe n pat2))))
> t7 = play (sim2 4)
>
> sim12 n = sim1 n :=: sim2 n
> t8 = play (sim12 4)
>
> sim3 n = Modify (Instrument Vibraphone)
>            (Modify (Transpose 48)
>               (Modify (Tempo 4) (simToHask (simFringe n pat3))))
> t9 = play (sim3 3)
>
> sim4 n  = Modify (Transpose 60)
>             (Modify (Tempo 2) (simToHask (simFringe n pat4')))
> 
> sim4s n = let s = sim4 n 
>               l1 = Modify (Instrument Flute) s
>               l2 = Modify (Instrument AcousticBass) 
>                      (Modify (Transpose (-36)) (revM s))
>           in l1 :=: l2
>
> ss     = sim4s 3
> durss  = dur ss
>
> t10    = play ss
>
> pat1,pat2,pat3,pat4,pat4' :: [SNote]
> pat1 = [[(0,1.0)],[(4,0.5)],[(7,1.0)],[(5,0.5)]]
> pat2 = [[(0,0.5)],[(4,1.0)],[(7,0.5)],[(5,1.0)]]
> pat3 = [[(2,0.6)],[(5,1.3)],[(0,1.0)],[(7,0.9)]]
> pat4' = [[(3,0.5)],[(4,0.25)],[(0,0.25)],[(6,1.0)]]
> pat4 = [[(3,0.5),(8,0.5),(22,0.5)],[(4,0.25),(7,0.25),(21,0.25)],
>         [(0,0.25),(5,0.25),(15,0.25)],[(6,1.0),(9,1.0),(19,1.0)]]

\end{verbatim} }
