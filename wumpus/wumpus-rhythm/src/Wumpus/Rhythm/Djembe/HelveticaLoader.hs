{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.GraphicInterpretation
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe
--
--------------------------------------------------------------------------------


module Wumpus.Rhythm.Djembe.HelveticaLoader
  (
    loadHelveticaMetrics
  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader




loadHelveticaMetrics :: FontDef -> IO (Either String FontLoadResult)
loadHelveticaMetrics fnt = 
   loadGS_helvetica fnt >>= maybe fk1 (return . Right)
 where
   fk1 = loadAfm_helvetica fnt >>= maybe fk2 (return . Right)
   fk2 = return $ Left help_message


help_message :: String
help_message = unlines $ 
    [ "This example uses glyph metrics loaded at runtime."
    , "It can use either the metrics files supplied with GhostScript,"
    , "or the AFM v4.1 metrics for the Core 14 fonts available from"
    , "Adobe's website."
    , "" 
    , "To use the Adode Core 14 font metrics download and unzip the "
    , "archive from the Adobe website and set the environment variable "
    , "WUMPUS_AFM_FONT_DIR to point to it."
    , ""
    , "To use GhostScripts font metrics set the environemt variable"
    , "WUMPUS_GS_FONT_DIR to point to the GhostScript fonts"
    , "directory (e.g. /usr/share/ghostscript/fonts)"
    , ""
    ]



loadGS_helvetica :: FontDef -> IO (Maybe FontLoadResult)
loadGS_helvetica fnt = 
    gsLoaderByEnv >>= maybe (return Nothing) (fmap Just . loadHelvetica fnt)


loadAfm_helvetica :: FontDef -> IO (Maybe FontLoadResult)
loadAfm_helvetica fnt = 
    afmLoaderByEnv >>= maybe (return Nothing) (fmap Just . loadHelvetica fnt)


loadHelvetica :: FontDef -> FontLoader -> IO FontLoadResult
loadHelvetica helv_def loader = do 
    metrics  <- loader [ Left helv_def ]
    printLoadErrors metrics
    return $ metrics

