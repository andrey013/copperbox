{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Slac.Supercolider.SynthDef.WriteFile
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC.
--
-- Output @synthdef@ files
--
--------------------------------------------------------------------------------


module Sound.Slac.Supercollider.SynthDef.WriteFile
  (

  -- * Write a scsynthdef file
    writeSynthDefFile

  ) where



import Sound.Slac.Supercollider.SynthDef.Datatypes
import Sound.Slac.Supercollider.SynthDef.Internal.IEEE754

import Data.Binary.Put                  -- package: binary

import qualified Data.ByteString.Lazy   as L
import Data.Char
import Data.Int
import System.IO


writeSynthDefFile :: FilePath -> SynthDefFile -> IO ()
writeSynthDefFile file_name defs = 
    openBinaryFile file_name WriteMode        >>= \hdl -> 
    L.hPut hdl (runPut $ putSynthDefFile defs)   >>
    hClose hdl 


putSynthDefFile :: SynthDefFile -> PutM ()
putSynthDefFile (SynthDefFile vnum defs) = do
    putString   "SCgf"
    putInt32be  vnum
    putInt16be  (fromIntegral $ length defs)
    mapM_ putSynthDef defs

putSynthDef :: SynthDef -> PutM ()
putSynthDef (SynthDef name konsts pvals pnames ugens)  = do
   putPString   name

   -- constants
   putInt16be   (fromIntegral $ length konsts)
   mapM_ putIEEESingle konsts
   
   -- params
   putInt16be   (fromIntegral $ length pvals)
   mapM_ putIEEESingle pvals

   -- param names
   putInt16be   (fromIntegral $ length pnames)
   mapM_ putParamName pnames

   -- ugens
   putInt16be   (fromIntegral $ length ugens)
   mapM_ putUgenSpec ugens


putParamName :: ParamName -> PutM ()
putParamName (ParamName name idx) = putPString name >> putInt16be idx
    

putUgenSpec :: UgenSpec -> PutM ()
putUgenSpec (UgenSpec name cr sidx inns outs) = do
   putPString   name
   putInt8      cr
   putInt16be   (fromIntegral $ length inns)
   putInt16be   (fromIntegral $ length outs)
   putInt16be   sidx
   mapM_ putInputSpec inns
   mapM_ putOutputSpec outs


putInputSpec :: InputSpec -> PutM ()
putInputSpec (InputKonst ixc)    = putInt16be (-1) >> putInt16be ixc
putInputSpec (InputUgen ixu ixo) = putInt16be ixu  >> putInt16be ixo

putOutputSpec :: OutputSpec -> PutM ()
putOutputSpec (OutputSpec rate) = putInt8 rate

--------------------------------------------------------------------------------
-- Helpers

putPString :: String -> PutM ()
putPString ss = putWord8 (fromIntegral $ length ss) >> putString ss 

putIEEESingle :: Double -> PutM ()
putIEEESingle dd = let (a,b,c,d) = packSingle dd in
    putWord8 a >> putWord8 b >> putWord8 c >> putWord8 d 

putInt8 :: Int8 -> PutM ()
putInt8 = putWord8 . fromIntegral



putInt16be :: Int16 -> PutM ()
putInt16be = putWord16be . fromIntegral

putInt32be :: Int32 -> PutM ()
putInt32be = putWord32be . fromIntegral

putString :: String -> PutM ()    
putString s = putLazyByteString (L.pack $ fmap (fromIntegral . ord) s) 