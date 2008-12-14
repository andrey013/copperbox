{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DocMidi
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Output functions for /hand-built/ Midi 
--
--------------------------------------------------------------------------------

module HNotate.DocMidi where

import HNotate.FPList
import HNotate.MiniMidi
import qualified HNotate.SequenceUtils as S
import HNotate.TemplateDatatypes

import Data.Sequence hiding (singleton)
import Data.Word

import Prelude hiding (null)


data MidiTemplate = MidiTemplate { 
      getMidiTemplate :: (HandBuiltMidi, [Maybe HoasExpr])
   }
   
   
type MessageS = Seq ExMessage -> Seq ExMessage

type BuildMidiS = BuildCombinedS (Seq ExMessage)

type HandBuiltMidi = FPList MessageS () 

-- Extended message - an effective but distasteful hack to 
-- put data that needs evaluating by the plug mechanism in 
-- the output sequence. 
data ExMessage = ExtMidi Message
               | TrackStart 
               | Delay DeltaTime
  deriving (Eq,Show)
            
midi :: [BuildMidiS] -> MidiTemplate
midi fs = MidiTemplate $ buildMidiContents fs

midi1 :: BuildMidiS -> MidiTemplate
midi1 f = MidiTemplate $ buildMidiContents [f]


-- Note, we always bookend the HandBuiltMidi with (<> emptyDoc)
-- this is to get a proper FPList (e.g. @abababa@ rather than @ababab@)    
buildMidiContents :: [BuildMidiS] -> (HandBuiltMidi, [Maybe HoasExpr])
buildMidiContents = combine . unzip . map midicontent where
    combine (ss,oes) = (makeMsgs ss, oes)
    -- see note above
    makeMsgs :: [Seq ExMessage -> Seq ExMessage] -> HandBuiltMidi 
    makeMsgs []     = singleton (>< empty)   
    makeMsgs (w:ws) = dblcons w () rest where rest = makeMsgs ws

    
midicontent :: BuildMidiS -> (Seq ExMessage -> Seq ExMessage, Maybe HoasExpr)
midicontent f = f ( (>< empty), Nothing)

newTrack :: BuildMidiS
newTrack = buildSrcOnlyS (|> msg) where
    msg = TrackStart
    
delay  :: Word32 -> BuildMidiS
delay n = buildSrcOnlyS (|> msg) where
    msg = Delay n    

genericText :: String -> BuildMidiS
genericText ss = buildSrcOnlyS (|> msg) where
    msg = ExtMidi $ midiMessage 0 (MetaText GENERIC_TEXT ss)

-- TODO - HNotate doesn't have an effective notion of channels
programChange :: Word8 -> Word8 -> BuildMidiS
programChange ch inst = buildSrcOnlyS (|> msg) where
    msg = ExtMidi $ midiMessage 0 (ProgramChange ch inst)


outputMidiEvents :: String -> BuildMidiS
outputMidiEvents name = buildExprOnlyS (ohdo directive) where
    directive = OutputDirective Nothing name


plugMidiTemplate :: FPList MessageS () -> [Seq ExMessage] -> Seq MidiTrack
plugMidiTemplate fpls ys =
    concatTracks $ merge applyS id $ knitOnB swap ys fpls
  where
    applyS :: MessageS -> Seq ExMessage
    applyS f = f empty
    
    swap :: a -> b -> b
    swap _ = id
    
concatTracks :: [Seq ExMessage] -> Seq MidiTrack
concatTracks = sfoldPt elemstep finalize empty empty . S.concat_ls's where  
    elemstep :: ExMessage -> Seq ExMessage -> Seq Message -> Seq (Seq Message)
                  -> (Seq ExMessage, Seq Message, Seq (Seq Message))
    elemstep TrackStart  se pt mass = (se, empty, mass|> pt)
  
    elemstep (Delay n)   se pt mass = (delay n (viewl se), pt, mass)
    
    elemstep (ExtMidi m) se pt mass = (se, pt|> m, mass)

    finalize :: Seq Message -> Seq (Seq Message) -> Seq MidiTrack 
    finalize se sse = fmap (\sa -> MidiTrack $ sa |> eot_msg) $ sse |> se
    
    delay :: Word32 -> ViewL ExMessage -> Seq ExMessage
    delay n ((ExtMidi (Message (dt,e))) :< sa) = a <| sa where
                                                 a = ExtMidi (Message (dt+n,e))
    delay _ (a :< sa)                          = a <| sa
    delay _ EmptyL                             = empty
    
                    



-- The double accumulator recursion pattern as a fold.
-- Note this general version also has lookahead - f4 the 
-- @element_step@ sees both the current element ans the 
-- remaining sequence.
 
sfoldPt ::(a -> Seq a -> b -> c -> (Seq a,b,c)) -> 
           (b -> c -> d) -> c -> b -> Seq a -> d
sfoldPt f4 f2 initial pt  = step initial pt . viewl where
  step acc ac EmptyL    = f2 ac acc
  step acc ac (a :< sa) = step acc' ac' (viewl sa') where 
                              (sa',ac',acc') = f4 a sa ac acc
                              
                                  




