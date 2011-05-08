{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.Supercolider.SynthDef.ReadFile
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC.
--
-- Parse @synthdef@ files.
--
-- Syndefs are big endian.
--
--------------------------------------------------------------------------------


module Sound.Slac.Supercollider.SynthDef.ReadFile
  (

  -- * Read a scsynthdef file
    readSynthDefFile

  -- * Auxiallary types
  , ParseErr
  , Pos
  , ErrMsg


  ) where


import Sound.Slac.Supercollider.SynthDef.Datatypes
import Sound.Slac.Supercollider.SynthDef.Internal.IEEE754
import Sound.Slac.Supercollider.SynthDef.Internal.ParserMonad

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy   as L
import Data.Int

readSynthDefFile :: FilePath -> IO (Either ParseErr SynthDefFile)
readSynthDefFile file_name =
    liftM (runParser `flip` synthDefFile) (L.readFile file_name)


synthDefFile :: ParserM SynthDefFile
synthDefFile = do 
    assertString "SCgf"
    vn <- int32be
    sc <- int16be
    dfs <- count (fromIntegral sc) synthDef
    return $ SynthDefFile 
                 { synth_version   = vn
                 , synth_defs      = dfs
                 }



synthDef :: ParserM SynthDef
synthDef = SynthDef <$> pstring <*> consts <*> params <*> pnames <*> ugens
  where
    consts = fmap fromIntegral int16be >>= count `flip` ieee754Single
    params = fmap fromIntegral int16be >>= count `flip` ieee754Single
    pnames = fmap fromIntegral int16be >>= count `flip` paramName
    ugens  = fmap fromIntegral int16be >>= count `flip` ugenSpec


paramName :: ParserM ParamName 
paramName = ParamName <$> pstring <*> int16be


ugenSpec :: ParserM UgenSpec
ugenSpec = do 
    nm  <- pstring
    cr  <- int8
    ic  <- int16be
    oc  <- int16be
    si  <- int16be
    ics <- count (fromIntegral ic) inputSpec
    ocs <- count (fromIntegral ic) outputSpec
    return $ UgenSpec 
                { ugen_name       = nm
                , ugen_calc_rate  = cr
                , ugen_kinput     = ic
                , ugen_koutput    = oc
                , ugen_sindex     = si
                , ugen_inputs     = ics
                , ugen_outputs    = ocs
                }

inputSpec :: ParserM InputSpec
inputSpec = int16be >>= step
  where
    step i | i == (-1) = InputKonst <$> int16be
           | otherwise = InputUgen  <$> int16be

outputSpec :: ParserM OutputSpec
outputSpec = OutputSpec <$> int8


-- | String prefixed by its length.
--
pstring :: ParserM String
pstring = getSize >>= \sz -> count sz char8
  where
    getSize = fmap fromIntegral word8


assertString :: String -> ParserM String
assertString s = postCheck (text $ length s) (==s) msg
  where
    msg a = "assertString - input: \"" ++ a ++ 
            "\" did not match expected: \"" ++ s ++ "\""


             
assertInt32 :: Integral a => a -> ParserM Int32
assertInt32 i = postCheck int32be ((==i) . fromIntegral) msg
  where
    msg a = "assertInt32 - input: "      ++ show a ++ 
            ", did not match expected: " ++ show i


-- | Apply parse then apply the check, if the check fails report
-- the error message. 
postCheck :: ParserM a -> (a -> Bool) -> (a -> String) -> ParserM a
postCheck p check msg = p >>= \ans -> 
    if check ans then return ans else reportError (msg ans)
