{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Base.FontLoadMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Font load monad handling IO (file system access), failure and 
-- logging.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.Base.FontLoadMonad
  (
    FontLoadErr
  , FontLoadIO
  , runFontLoadIO
  , evalFontLoadIO
  , loadError
  , logLoadMsg
  , promoteIO
  , promoteEither
  , runParserFLIO

  , sequenceAll

  -- * Font loading

  , buildAfmFontProps
  , checkFontPath
  
  ) where

import Wumpus.Basic.System.FontLoader.Base.Datatypes
import Wumpus.Basic.Utils.HList
import Wumpus.Basic.Utils.ParserCombinators


import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices


import Control.Monad
import qualified Data.IntMap            as IntMap
import qualified Data.Map as Map
import Data.Monoid
import System.Directory
import System.FilePath



--------------------------------------------------------------------------------
-- FontLoadIO monad - IO plus Error



type FontLoadErr        = String

newtype FontLoadLog     = FontLoadLog { getFontLoadLog :: H String }


instance Monoid FontLoadLog where
  mempty        = FontLoadLog $ emptyH
  a `mappend` b = FontLoadLog $ getFontLoadLog a `appendH` getFontLoadLog b



newtype FontLoadIO a = FontLoadIO { 
          getFontLoadIO :: IO (Either FontLoadErr a, FontLoadLog ) }

instance Functor FontLoadIO where
  fmap f ma = FontLoadIO $ getFontLoadIO ma >>= \(a,w) -> return (fmap f a, w)
 
instance Monad FontLoadIO where
  return a = FontLoadIO $ return (Right a, mempty)
  m >>= k  = FontLoadIO $ getFontLoadIO m >>= fn 
              where
                fn (Left err, w) = return (Left err, w)
                fn (Right a, w1) = getFontLoadIO (k a) >>= \(b,w2) -> 
                                   return (b, w1 `mappend` w2)

runFontLoadIO :: FontLoadIO a -> IO (Either FontLoadErr a,[String])
runFontLoadIO ma = liftM post $ getFontLoadIO ma 
  where
    post (ans,w) = (ans, toListH $ getFontLoadLog w)


evalFontLoadIO :: FontLoadIO a -> IO (Either FontLoadErr a)
evalFontLoadIO ma = liftM post $ getFontLoadIO ma
  where
    post (ans,_) = ans


loadError :: FontLoadErr -> FontLoadIO a
loadError msg = FontLoadIO $ return (Left msg, mempty)

logLoadMsg :: String -> FontLoadIO ()
logLoadMsg msg = FontLoadIO $ return (Right (), message1 msg ) 


message1 :: String -> FontLoadLog 
message1 = FontLoadLog . wrapH


-- | aka liftIO
promoteIO :: IO a -> FontLoadIO a
promoteIO ma = FontLoadIO $ ma >>= \a -> return (Right a, mempty)

promoteEither :: Either FontLoadErr a -> FontLoadIO a
promoteEither = either loadError return 

runParserFLIO :: FilePath -> Parser Char a -> FontLoadIO a
runParserFLIO filepath p = 
   promoteIO (readFile filepath) >>= promoteEither . runParserEither p


-- | The standard monadic @sequence@ would finish on first fail
-- for the FontLoadIO monad. As we want to be able to sequence
-- the loading of a list of fonts, this is not really the 
-- behaviour we want for Wumpus. Instead we prefer to use fallback 
-- metrics and produce an inaccurate drawing on a font load error
-- rather than fail and produce no drawing.
--
sequenceAll :: [FontLoadIO a] -> FontLoadIO [a]
sequenceAll = FontLoadIO . step
   where
    step []     = return (Right [], mempty)
    step (m:ms) = liftM2 cons (getFontLoadIO m) (step ms) 

cons :: (Either FontLoadErr a, FontLoadLog)
     -> (Either FontLoadErr [a], FontLoadLog)
     -> (Either FontLoadErr [a], FontLoadLog)
cons (Right a, w1)  (Right as, w2) = 
    (Right $ a:as,  w1 `mappend` w2)

cons (Right a, w1)  (Left e2, w2) = 
    (Right [a], w1 `mappend` w2 `mappend` message1 e2)

cons (Left e1, w1)  (Right as, w2) = 
    (Right as, w1 `mappend` message1 e1 `mappend` w2)

cons (Left e1, w1)  (Left e2,  w2) = 
    (Right [], w1 `mappend` message1 e1 `mappend` w2 `mappend` message1 e2)



--------------------------------------------------------------------------------


-- | Afm files do not have a default advance vec so use the 
-- monospace default.
-- 
-- Afm files hopefully have @CapHeight@ and @FontBBox@ properties
-- in the header. Use the monospace default only if they are 
-- missing.
-- 
buildAfmFontProps :: MonospaceDefaults AfmUnit 
                  -> AfmFile 
                  -> FontLoadIO (FontProps AfmUnit)
buildAfmFontProps defaults afm = do 
    cap_height <- extractCapHeight defaults afm
    bbox       <- extractFontBBox  defaults afm 
    return $ FontProps 
               { fp_bounding_box    = bbox
               , fp_default_adv_vec = default_char_width defaults
               , fp_adv_vecs        = char_widths
               , fp_cap_height      = cap_height
               }  
  where
    char_widths = foldr fn IntMap.empty $ afm_glyph_metrics afm
 
    fn (AfmGlyphMetrics _ v ss) table = case Map.lookup ss ps_glyph_indices of
                                          Nothing -> table
                                          Just i  -> IntMap.insert i v table


extractCapHeight :: MonospaceDefaults AfmUnit -> AfmFile -> FontLoadIO AfmUnit
extractCapHeight defaults afm = maybe errk return $ afm_cap_height afm
  where
    errk = logLoadMsg "WARNING - Could not extract CapHeight" >> 
           return (default_cap_height defaults)


extractFontBBox :: MonospaceDefaults AfmUnit -> AfmFile 
                -> FontLoadIO (BoundingBox AfmUnit)
extractFontBBox defaults afm = maybe errk return $ afm_letter_bbox afm
  where
    errk = logLoadMsg "WARNING - Could not extract CapHeight" >> 
           return (default_letter_bbox defaults)



checkFontPath :: FilePath -> FilePath -> FontLoadIO FilePath
checkFontPath path_root font_file_name = 
    let full_path = normalise (path_root </> font_file_name)
    in do { check <- promoteIO (doesFileExist full_path)
          ; if check then return full_path
                     else loadError $ "Could not resolve path: " ++ full_path
          }
