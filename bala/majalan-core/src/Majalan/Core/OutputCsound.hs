{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.OutputCsound
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Csound - scores (.sco) or unified orchestra and score 
-- files (.csd). 
--
--------------------------------------------------------------------------------

module Majalan.Core.OutputCsound
  (

    writeSco
  , writeUnifiedFile
  , writeUnifiedScore

  , CsoundFlags(..)
  , flags_rt_audio

  , flags_wav_file_out
  , flags_aiff_file_out
  , flags_display

  ) where

import Majalan.Core.Internal.CSDDoc
import Majalan.Core.Score

import Text.PrettyPrint.HughesPJ


-- | Write a score (@.sco@) file.
--
writeSco :: FilePath -> ColumnSpecs -> Score -> IO ()
writeSco file_path cols xs = 
    writeFile file_path $ show $ buildScoreDoc cols xs




-- | Write a unified @.csd@ file where the orchestra is 
-- represented as Strings (e.g. the output of some generator).
-- 
writeUnifiedFile :: FilePath 
                 -> CsoundFlags 
                 -> [String] 
                 -> ColumnSpecs 
                 -> Score 
                 -> IO ()
writeUnifiedFile file_path flags insts cols sco = 
    writeFile file_path $ show $ csound_synthesizer dflags orch dsco
  where
    dflags = cs_options [getCsoundFlags flags]
    orch   = cs_instruments $ map text insts
    dsco   = cs_score $ buildScoreDoc cols sco



-- | Write a unified @.csd@ file incorporating a pre-written
-- orchestra file.
-- 
-- The orchestra file is expected to dictate sample-rate, number
-- of channels, etc.
--
writeUnifiedScore :: FilePath 
                  -> CsoundFlags 
                  -> FilePath 
                  -> ColumnSpecs 
                  -> Score 
                  -> IO ()
writeUnifiedScore out_path flags orch_file cols sco = do
    orch <- readFile orch_file
    writeFile out_path $ show $ csound_synthesizer dflags (mkOrch orch) dsco
  where
    dflags = cs_options [getCsoundFlags flags]
    mkOrch = \ss -> cs_instruments [ text ss ]
    dsco   = cs_score $ buildScoreDoc cols sco




data CsoundFlags = CsoundFlags { getCsoundFlags :: String }
  deriving (Eq,Ord,Show)

flags_rt_audio :: CsoundFlags
flags_rt_audio = CsoundFlags "-odac     ;;; RT audio out"


-- | Note - file path is passed to the shell.
-- 
flags_wav_file_out :: FilePath -> CsoundFlags
flags_wav_file_out wav = CsoundFlags $ body 
  where
    body = "-o " ++ wav ++ " -W"


-- | Note - file path is passed to the shell.
-- 
flags_aiff_file_out :: FilePath -> CsoundFlags
flags_aiff_file_out aiff = CsoundFlags $ body 
  where
    body = "-o " ++ aiff ++ " -A"


-- | Enable display for @display@ opcode, no audio is generated.
-- 
flags_display :: CsoundFlags
flags_display = CsoundFlags $ body 
  where
    body = "--displays"

