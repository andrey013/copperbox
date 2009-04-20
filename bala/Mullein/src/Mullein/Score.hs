{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Score
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Named musical elements (e.g. notes, keys) within the NoteCtx monad
--
--------------------------------------------------------------------------------

module Mullein.Score (
    ( & ),
    evaluatePart,

    part,

    repeated, fsrepeat,
    
    motif,
    primary, addOverlay,
    notelist,

    rest, space,

    c4, d4, e4, f4, g4, a4, b4,
    cis4, des4, dis4, ees4, fis4, ges4, gis4, aes4, ais4, bes4,
    
    c3, d3, e3, f3, g3, a3, b3,
    cis3, des3, dis3, ees3, fis3, ges3, gis3, aes3, ais3, bes3,
  
    c2, d2, e2, f2, g2, a2, b2,
    cis2, des2, dis2, ees2, fis2, ges2, gis2, aes2, ais2, bes2,
  
    c1, d1, e1, f1, g1, a1, b1,
    cis1, des1, dis1, ees1, fis1, ges1, gis1, aes1, ais1, bes1,
        
    c5, d5, e5, f5, g5, a5, b5,
    cis5, des5, dis5, ees5, fis5, ges5, gis5, aes5, ais5, bes5,
    
    c6, d6, e6, f6, g6, a6, b6,
    cis6, des6, dis6, ees6, fis6, ges6, gis6, aes6, ais6, bes6,
  
    c7, d7, e7, f7, g7, a7, b7,
    cis7, des7, dis7, ees7, fis7, ges7, gis7, aes7, ais7, bes7,
    
    
    -- * Named durations (Shorthand)
    dot, 
    du1, du2, du4, du8, du16, du32, du64, du128,  
    dotdu1, dotdu2, dotdu4, dotdu8, dotdu16, dotdu32, dotdu64, dotdu128,  
    
 ) where

import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import qualified Mullein.Duration as D 
import Mullein.ScoreDatatypes
import Mullein.Pitch
import Mullein.RS

import Control.Applicative
import Data.Ratio


type ElementP = Element Pitch

(&) :: Monad m => m a -> m b -> m a
(&) f upd  = upd >> f 


--------------------------------------------------------------------------------
-- build the score


evaluatePart :: Key -> MetricalSpec -> NoteCtx (Part e) -> Part e
evaluatePart key mspec mpart = evalRS mpart  st0 env0 where
    st0  = St { prev_note_length = 1%4,
                metrical_spec    = mspec,
                current_key      = key }
    env0 = Env



part :: [NoteCtx (Phrase e)] -> NoteCtx (Part e)
part ms = Part <$> sequence ms

repeated :: NoteCtx (Motif e) -> NoteCtx (Phrase e)
repeated ma = Repeated <$> ma

fsrepeat :: NoteCtx (Motif e) 
         -> NoteCtx (Motif e) 
         -> NoteCtx (Motif e)
         -> NoteCtx (Phrase e)
fsrepeat ma mx my = FSRepeat <$> ma <*> mx <*> my 

motif :: NoteCtx (OverlayList e) -> NoteCtx (Motif e)
motif ovs = bracket 
    <$> gets current_key <*> gets metrical_spec <*> ovs


primary :: NoteCtx (ElemList e) -> NoteCtx (OverlayList e)
primary ms = (\xs -> (xs,[])) <$> ms

addOverlay :: BarNum -> NoteCtx (ElemList e) -> NoteCtx (OverlayList e)
           -> NoteCtx (OverlayList e)
addOverlay n ns os = (\xs (p,xss) -> (p,(n,xs):xss)) <$> ns <*> os


notelist :: [NoteCtx e] -> NoteCtx [e]
notelist ms = (sequence ms) & modify (\s -> s { prev_note_length = 1%4 })


--


rest :: NoteCtx (Element e)
rest = gets prev_note_length >>= return . Rest

space :: NoteCtx (Element e)
space = gets prev_note_length >>= return . Spacer


noteNat :: PitchLetter -> Int -> NoteCtx (Element Pitch)
noteNat n o    = gets prev_note_length >>= return . Note (Pitch n Nat o)

noteSharp :: PitchLetter -> Int -> NoteCtx (Element Pitch)
noteSharp n o  = gets prev_note_length >>= return . Note (Pitch n Sharp o)

noteFlat :: PitchLetter -> Int -> NoteCtx (Element Pitch)
noteFlat n o   = gets prev_note_length >>= return . Note (Pitch n Flat o)


c4    :: NoteCtx (Element Pitch)
d4    :: NoteCtx (Element Pitch) 
e4    :: NoteCtx (Element Pitch)
f4    :: NoteCtx (Element Pitch)
g4    :: NoteCtx (Element Pitch)
a4    :: NoteCtx (Element Pitch) 
b4    :: NoteCtx (Element Pitch) 
cis4  :: NoteCtx (Element Pitch) 
des4  :: NoteCtx (Element Pitch) 
dis4  :: NoteCtx (Element Pitch) 
ees4  :: NoteCtx (Element Pitch)
fis4  :: NoteCtx (Element Pitch)
ges4  :: NoteCtx (Element Pitch) 
gis4  :: NoteCtx (Element Pitch) 
aes4  :: NoteCtx (Element Pitch) 
ais4  :: NoteCtx (Element Pitch) 
bes4  :: NoteCtx (Element Pitch)
c4    = noteNat C 4
d4    = noteNat D 4
e4    = noteNat E 4
f4    = noteNat F 4
g4    = noteNat G 4
a4    = noteNat A 4
b4    = noteNat B 4
cis4  = noteSharp C 4
des4  = noteFlat D 4
dis4  = noteSharp D 4
ees4  = noteFlat E 4
fis4  = noteSharp F 4
ges4  = noteFlat G 4
gis4  = noteSharp G 4
aes4  = noteFlat A 4
ais4  = noteSharp A 4
bes4  = noteFlat B 4


c3    :: NoteCtx (Element Pitch)
d3    :: NoteCtx (Element Pitch) 
e3    :: NoteCtx (Element Pitch)
f3    :: NoteCtx (Element Pitch)
g3    :: NoteCtx (Element Pitch)
a3    :: NoteCtx (Element Pitch) 
b3    :: NoteCtx (Element Pitch) 
cis3  :: NoteCtx (Element Pitch) 
des3  :: NoteCtx (Element Pitch) 
dis3  :: NoteCtx (Element Pitch) 
ees3  :: NoteCtx (Element Pitch)
fis3  :: NoteCtx (Element Pitch)
ges3  :: NoteCtx (Element Pitch) 
gis3  :: NoteCtx (Element Pitch) 
aes3  :: NoteCtx (Element Pitch) 
ais3  :: NoteCtx (Element Pitch) 
bes3  :: NoteCtx (Element Pitch)
c3    = noteNat C 3
d3    = noteNat D 3
e3    = noteNat E 3
f3    = noteNat F 3
g3    = noteNat G 3
a3    = noteNat A 3
b3    = noteNat B 3
cis3  = noteSharp C 3
des3  = noteFlat D 3
dis3  = noteSharp D 3
ees3  = noteFlat E 3
fis3  = noteSharp F 3
ges3  = noteFlat G 3
gis3  = noteSharp G 3
aes3  = noteFlat A 3
ais3  = noteSharp A 3
bes3  = noteFlat B 3

c2    :: NoteCtx (Element Pitch)
d2    :: NoteCtx (Element Pitch) 
e2    :: NoteCtx (Element Pitch)
f2    :: NoteCtx (Element Pitch)
g2    :: NoteCtx (Element Pitch)
a2    :: NoteCtx (Element Pitch) 
b2    :: NoteCtx (Element Pitch) 
cis2  :: NoteCtx (Element Pitch) 
des2  :: NoteCtx (Element Pitch) 
dis2  :: NoteCtx (Element Pitch) 
ees2  :: NoteCtx (Element Pitch)
fis2  :: NoteCtx (Element Pitch)
ges2  :: NoteCtx (Element Pitch) 
gis2  :: NoteCtx (Element Pitch) 
aes2  :: NoteCtx (Element Pitch) 
ais2  :: NoteCtx (Element Pitch) 
bes2  :: NoteCtx (Element Pitch)
c2    = noteNat C 2
d2    = noteNat D 2
e2    = noteNat E 2
f2    = noteNat F 2
g2    = noteNat G 2
a2    = noteNat A 2
b2    = noteNat B 2
cis2  = noteSharp C 2
des2  = noteFlat D 2
dis2  = noteSharp D 2
ees2  = noteFlat E 2
fis2  = noteSharp F 2
ges2  = noteFlat G 2
gis2  = noteSharp G 2
aes2  = noteFlat A 2
ais2  = noteSharp A 2
bes2  = noteFlat B 2

c1    :: NoteCtx (Element Pitch)
d1    :: NoteCtx (Element Pitch) 
e1    :: NoteCtx (Element Pitch)
f1    :: NoteCtx (Element Pitch)
g1    :: NoteCtx (Element Pitch)
a1    :: NoteCtx (Element Pitch) 
b1    :: NoteCtx (Element Pitch) 
cis1  :: NoteCtx (Element Pitch) 
des1  :: NoteCtx (Element Pitch) 
dis1  :: NoteCtx (Element Pitch) 
ees1  :: NoteCtx (Element Pitch)
fis1  :: NoteCtx (Element Pitch)
ges1  :: NoteCtx (Element Pitch) 
gis1  :: NoteCtx (Element Pitch) 
aes1  :: NoteCtx (Element Pitch) 
ais1  :: NoteCtx (Element Pitch) 
bes1  :: NoteCtx (Element Pitch)
c1    = noteNat C 1
d1    = noteNat D 1
e1    = noteNat E 1
f1    = noteNat F 1
g1    = noteNat G 1
a1    = noteNat A 1
b1    = noteNat B 1
cis1  = noteSharp C 1
des1  = noteFlat D 1
dis1  = noteSharp D 1
ees1  = noteFlat E 1
fis1  = noteSharp F 1
ges1  = noteFlat G 1
gis1  = noteSharp G 1
aes1  = noteFlat A 1
ais1  = noteSharp A 1
bes1  = noteFlat B 1


c5    :: NoteCtx (Element Pitch)
d5    :: NoteCtx (Element Pitch) 
e5    :: NoteCtx (Element Pitch)
f5    :: NoteCtx (Element Pitch)
g5    :: NoteCtx (Element Pitch)
a5    :: NoteCtx (Element Pitch) 
b5    :: NoteCtx (Element Pitch) 
cis5  :: NoteCtx (Element Pitch) 
des5  :: NoteCtx (Element Pitch) 
dis5  :: NoteCtx (Element Pitch) 
ees5  :: NoteCtx (Element Pitch)
fis5  :: NoteCtx (Element Pitch)
ges5  :: NoteCtx (Element Pitch) 
gis5  :: NoteCtx (Element Pitch) 
aes5  :: NoteCtx (Element Pitch) 
ais5  :: NoteCtx (Element Pitch) 
bes5  :: NoteCtx (Element Pitch)
c5    = noteNat C 5
d5    = noteNat D 5
e5    = noteNat E 5
f5    = noteNat F 5
g5    = noteNat G 5
a5    = noteNat A 5
b5    = noteNat B 5
cis5  = noteSharp C 5
des5  = noteFlat D 5
dis5  = noteSharp D 5
ees5  = noteFlat E 5
fis5  = noteSharp F 5
ges5  = noteFlat G 5
gis5  = noteSharp G 5
aes5  = noteFlat A 5
ais5  = noteSharp A 5
bes5  = noteFlat B 5

c6    :: NoteCtx (Element Pitch)
d6    :: NoteCtx (Element Pitch) 
e6    :: NoteCtx (Element Pitch)
f6    :: NoteCtx (Element Pitch)
g6    :: NoteCtx (Element Pitch)
a6    :: NoteCtx (Element Pitch) 
b6    :: NoteCtx (Element Pitch) 
cis6  :: NoteCtx (Element Pitch) 
des6  :: NoteCtx (Element Pitch) 
dis6  :: NoteCtx (Element Pitch) 
ees6  :: NoteCtx (Element Pitch)
fis6  :: NoteCtx (Element Pitch)
ges6  :: NoteCtx (Element Pitch) 
gis6  :: NoteCtx (Element Pitch) 
aes6  :: NoteCtx (Element Pitch) 
ais6  :: NoteCtx (Element Pitch) 
bes6  :: NoteCtx (Element Pitch)
c6    = noteNat C 6
d6    = noteNat D 6
e6    = noteNat E 6
f6    = noteNat F 6
g6    = noteNat G 6
a6    = noteNat A 6
b6    = noteNat B 6
cis6  = noteSharp C 6
des6  = noteFlat D 6
dis6  = noteSharp D 6
ees6  = noteFlat E 6
fis6  = noteSharp F 6
ges6  = noteFlat G 6
gis6  = noteSharp G 6
aes6  = noteFlat A 6
ais6  = noteSharp A 6
bes6  = noteFlat B 6

c7    :: NoteCtx (Element Pitch)
d7    :: NoteCtx (Element Pitch) 
e7    :: NoteCtx (Element Pitch)
f7    :: NoteCtx (Element Pitch)
g7    :: NoteCtx (Element Pitch)
a7    :: NoteCtx (Element Pitch) 
b7    :: NoteCtx (Element Pitch) 
cis7  :: NoteCtx (Element Pitch) 
des7  :: NoteCtx (Element Pitch) 
dis7  :: NoteCtx (Element Pitch) 
ees7  :: NoteCtx (Element Pitch)
fis7  :: NoteCtx (Element Pitch)
ges7  :: NoteCtx (Element Pitch) 
gis7  :: NoteCtx (Element Pitch) 
aes7  :: NoteCtx (Element Pitch) 
ais7  :: NoteCtx (Element Pitch) 
bes7  :: NoteCtx (Element Pitch)
c7    = noteNat C 7
d7    = noteNat D 7
e7    = noteNat E 7
f7    = noteNat F 7
g7    = noteNat G 7
a7    = noteNat A 7
b7    = noteNat B 7
cis7  = noteSharp C 7
des7  = noteFlat D 7
dis7  = noteSharp D 7
ees7  = noteFlat E 7
fis7  = noteSharp F 7
ges7  = noteFlat G 7
gis7  = noteSharp G 7
aes7  = noteFlat A 7
ais7  = noteSharp A 7
bes7  = noteFlat B 7



-- Unit note length transformers
dot :: NoteCtx ()
dot = do
    d <- gets prev_note_length
    modify $ \s -> s { prev_note_length = D.dot d }
    
    
du1   :: NoteCtx ()
du1   = modify $ \s -> s {prev_note_length = 1%1}

du2   :: NoteCtx ()
du2   = modify $ \s -> s {prev_note_length = 1%2}

du4   :: NoteCtx ()
du4   = modify $ \s -> s {prev_note_length = 1%4}

du8   :: NoteCtx ()
du8   = modify $ \s -> s {prev_note_length = 1%8}

du16  :: NoteCtx ()
du16  = modify $ \s -> s {prev_note_length = 1%16}

du32  :: NoteCtx ()
du32  = modify $ \s -> s {prev_note_length = 1%32}

du64  :: NoteCtx ()
du64  = modify $ \s -> s {prev_note_length = 1%64}

du128 :: NoteCtx ()
du128 = modify $ \s -> s {prev_note_length = 1%128}


dotdu1   :: NoteCtx ()
dotdu1   = modify $ \s -> s {prev_note_length = D.dot $ 1%1}

dotdu2   :: NoteCtx ()
dotdu2   = modify $ \s -> s {prev_note_length = D.dot $ 1%2}

dotdu4   :: NoteCtx ()
dotdu4   = modify $ \s -> s {prev_note_length = D.dot $ 1%4}

dotdu8   :: NoteCtx ()
dotdu8   = modify $ \s -> s {prev_note_length = D.dot $ 1%8}

dotdu16  :: NoteCtx ()
dotdu16  = modify $ \s -> s {prev_note_length = D.dot $ 1%16}

dotdu32  :: NoteCtx ()
dotdu32  = modify $ \s -> s {prev_note_length = D.dot $ 1%32}

dotdu64  :: NoteCtx ()
dotdu64  = modify $ \s -> s {prev_note_length = D.dot $ 1%64}

dotdu128 :: NoteCtx ()
dotdu128 = modify $ \s -> s {prev_note_length = D.dot $ 1%128}


