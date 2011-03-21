{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Top level module for font loading...
--
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader
  (
    FontLoader
  , afmLoaderByEnv
  , gsLoaderByEnv
  , defaultFontLoader

  , processCmdLine
  , default_font_loader_help
  ) where

-- @FontSupport@ might be a better name
import Wumpus.Basic.Kernel.Base.FontMetrics     
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript

import Control.Applicative
import Control.Monad
import System.Directory
import System.Console.GetOpt
import System.Environment
import System.IO.Error


-- | A FontLoader is an action from a list of fonts to a
-- 'FontLoadResult' returned in @IO@.
-- 
type FontLoader = [FontName] -> IO FontLoadResult


-- | Environment variable pointing to the GhostScript font
-- directory.
-- 
-- > WUMPUS_GS_FONT_DIR
--
wumpus_gs_font_dir :: String
wumpus_gs_font_dir = "WUMPUS_GS_FONT_DIR"


-- | Environment variable pointing to the diretory containing 
-- the Adobe Font Metrics files.
-- 
-- > WUMPUS_AFM_FONT_DIR
--
wumpus_afm_font_dir :: String
wumpus_afm_font_dir = "WUMPUS_AFM_FONT_DIR"



afmLoaderByEnv :: IO (Maybe FontLoader)
afmLoaderByEnv =
    envLookup wumpus_afm_font_dir >>= return . fmap loadAfmFontMetrics


gsLoaderByEnv :: IO (Maybe FontLoader)
gsLoaderByEnv =
    envLookup wumpus_gs_font_dir >>= return . fmap loadGSFontMetrics


-- | Tries to find the GhostScript metrics first...
--
-- Runs the IO action on the loader if it finds one.
--
defaultFontLoader :: (FontLoader -> IO a) ->  IO (Maybe a)
defaultFontLoader mf = 
    gsLoaderByEnv >>= maybe fk1 sk 
  where
   fk1       = afmLoaderByEnv >>= maybe fk2 sk
   fk2       = putStrLn default_font_loader_help >> return Nothing
   sk loader = mf loader >>= return . Just

   


default_font_loader_help :: String
default_font_loader_help = unlines $ 
    [ "This example uses glyph metrics loaded at runtime."
    , "It can use either the metrics files supplied with GhostScript,"
    , "or the AFM v4.1 metrics for the Core 14 fonts available from"
    , "Adobe's website."
    , "" 
    , "To use GhostScripts font metrics set the environemt variable"
    , wumpus_gs_font_dir ++ " to point to the GhostScript fonts"
    , "directory (e.g. /usr/share/ghostscript/fonts)."
    , ""
    , "To use the Adode Core 14 font metrics download the archive from"
    , "the Adobe website and set the environment variable "
    , wumpus_afm_font_dir ++ " to point to it."
    , ""
    , "If you have both environment variables set, the GhostScript loader"
    , "will be used."
    ]


data CmdLineFlag = Help
                 | GS_FontDir  String
                 | AFM_FontDir String
  deriving (Eq,Ord,Show)

processCmdLine :: String -> IO (Maybe FilePath, Maybe FilePath)
processCmdLine help_message = 
    let options = makeCmdLineOptions help_message in do
        args <- getArgs
        let (opts, _, _) = getOpt Permute options args
        if Help `elem` opts then failk help_message
                            else succk opts
  where
    failk msg   = putStr msg >> return (Nothing,Nothing) 
    succk flags = (,) <$> gsFontDirectory flags <*> afmFontDirectory flags 
       

makeCmdLineOptions :: String -> [OptDescr CmdLineFlag]
makeCmdLineOptions help_message =
    [ Option ['h'] ["help"]   (NoArg Help)                help_message
    , Option []    ["afm"]    (ReqArg AFM_FontDir "DIR")  "AFM v4.1 metrics dir"
    , Option []    ["gs"]     (ReqArg GS_FontDir  "DIR")  "GhoshScript font dir"
    ]


gsFontDirectory :: [CmdLineFlag] -> IO (Maybe FilePath)
gsFontDirectory = step 
  where
    step (GS_FontDir p:xs)  = doesDirectoryExist p >>= \check -> 
                              if check then return (Just p) else step xs

    step (_:xs)             = step xs
    step []                 = envLookup wumpus_gs_font_dir
 

afmFontDirectory :: [CmdLineFlag] -> IO (Maybe FilePath)
afmFontDirectory = step 
  where
    step (AFM_FontDir p:xs) = doesDirectoryExist p >>= \check -> 
                              if check then return (Just p) else step xs

    step (_:xs)             = step xs
    step []                 = envLookup wumpus_afm_font_dir


envLookup :: String -> IO (Maybe String)
envLookup name = liftM fn $ try $ getEnv name
  where
    fn (Left _)  = Nothing
    fn (Right a) = Just a

