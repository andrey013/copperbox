
--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.TimbalesStyle
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- TimbalesStyle - output LilyPond percusion notation in #timbales-style
--
--------------------------------------------------------------------------------


module Bala.MusicRep.TimbalesStyle where

import Bala.Base.DrumOutput
import Bala.Base.Duration
import Bala.Base.OutputHNotate (EventFoldStep, genDrumFoldStep)
import Bala.Base.Pitch

import qualified HNotate.NoteListDatatypes as H
import qualified HNotate.Marks as H 
import qualified ZMidi as Z (GMDrum(..), drumPitch)



data TimbalesStyle = Losidestick
                   | Lotimbale
                   | Cowbell
                   | Hisidestick
                   | Hitimbale
  deriving (Enum,Eq,Ord,Show)
  
  
instance DrumMapping TimbalesStyle where
  gmDrum Losidestick        = id $ Z.Side_stick
  gmDrum Lotimbale          = Z.Low_timbale
  gmDrum Cowbell            = Z.Cowbell
  gmDrum Hisidestick        = id $ Z.Chinese_cymbal
  gmDrum Hitimbale          = Z.High_timbale

  drumName = maybe Nothing fn . pitchToGmDrum where
    fn Z.Side_stick             = id $ Just Losidestick
    fn Z.Low_timbale            = Just Lotimbale 
    fn Z.Cowbell                = Just Cowbell 
    fn Z.Chinese_cymbal         = id $ Just Hisidestick 
    fn Z.High_timbale           = Just Hitimbale      
    fn _                        = Nothing

drumMark :: TimbalesStyle -> H.Mark H.DrumMark 
drumMark Losidestick          = H.losidestick'   
drumMark Lotimbale            = H.lotimbale'           
drumMark Cowbell              = H.cowbell'          
drumMark Hisidestick          = H.hisidestick'      
drumMark Hitimbale            = H.hitimbale'     

drumFoldStep :: EventFoldStep
drumFoldStep = genDrumFoldStep drumMark 

   