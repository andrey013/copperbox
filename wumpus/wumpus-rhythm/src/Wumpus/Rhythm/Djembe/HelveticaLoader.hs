{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.GraphicInterpretation
-- Copyright   :  (c) Stephen Tetley 2010
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
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript


import Control.Applicative
import Control.Monad
import System.Environment
import System.IO.Error ( try )

wumpus_gs_font_dir :: String
wumpus_gs_font_dir = "WUMPUS_GS_FONT_DIR"

wumpus_afm_font_dir :: String
wumpus_afm_font_dir = "WUMPUS_AFM_FONT_DIR"


loadHelveticaMetrics :: IO (Either String GlyphMetrics)
loadHelveticaMetrics = 
   loadAfm_helvetica >>= maybe fk1 (return . Right)
 where
   fk1 = loadGS_helvetica >>= maybe (return $ Left help_message) (return . Right)


help_message :: String
help_message = unlines $ 
    [ "This example uses glyph metrics loaded at runtime."
    , "It can use either the metrics files supplied with GhostScript,"
    , "or the AFM v4.1 metrics for the Core 14 fonts available from"
    , "Adobe's website."
    , "" 
    , "To use the Adode Core 14 font metrics download and unzip the "
    , "archive from the Adobe website and set the environment variable "
    , wumpus_afm_font_dir ++ " to point to it."
    , ""
    , "To use GhostScripts font metrics set the environemt variable"
    , wumpus_gs_font_dir ++ " to point to the GhostScript fonts"
    , "directory (e.g. /usr/share/ghostscript/fonts)"
    , ""
    ]



loadGS_helvetica :: IO (Maybe GlyphMetrics)
loadGS_helvetica = 
    envLookup wumpus_gs_font_dir >>=
    maybe (return Nothing) (\dir -> Just <$> loadGSMetrics dir ["Helvetica"])

loadAfm_helvetica :: IO (Maybe GlyphMetrics)
loadAfm_helvetica = 
    envLookup wumpus_afm_font_dir >>=
    maybe (return Nothing) (\dir -> Just <$> loadAfmMetrics dir ["Helvetica"])


envLookup :: String -> IO (Maybe String)
envLookup name = liftM fn $ try $ getEnv name
  where
    fn (Left _)  = Nothing
    fn (Right a) = Just a

