{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.ParseFont
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interface to the /jumping/ position parser combinators  
-- 
--------------------------------------------------------------------------------

module Graphics.SFont.ParseFont where

import Graphics.SFont.Decode
import Graphics.SFont.ExtraSyntax
import Graphics.SFont.GlyphDecoder
import Graphics.SFont.Parse
import Graphics.SFont.PrimitiveDatatypes
import Graphics.SFont.Syntax
import Graphics.SFont.Utils

import Control.Applicative
import Control.Monad.State
import Data.Array.IO ( IOUArray, hGetArray, newArray_, freeze )
import Data.Word
import System.IO ( IOMode(..), withBinaryFile, hFileSize ) 

-- FontLoader is a State Monad transformed by ParserT   
type TtffParser r a = ParserT r (State TtffParseState) a


-- To parse a ttf or otf file we need to see a lot of intermediate information
-- which isn't reflected in the final /parse tree/. We build this information
-- as we parse with the state monad. It doesn't seem very elegant to have 
-- all of the initial state fields undefined, but...   
data TtffParseState = TtffParseState 
        { table_count     :: Int
        , table_locs      :: TableLocs
        , glyph_locs      :: [Region]
        }
  deriving (Show)

evalParseTTFF :: FilePath -> IO (Either String TTFF)
evalParseTTFF path = fst <$> runParseTTFF path


-- for debugging its handy to be able to view the state

runParseTTFF :: FilePath -> IO (Either String TTFF, TtffParseState)
runParseTTFF path = withBinaryFile path ReadMode $ \h -> do
    sz_i    <- hFileSize h
    marr    <- newArray_ (mkBounds sz_i) 
    sz_r    <- hGetArray h marr (fromIntegral sz_i)
    arr     <- freezeByteSequence marr
    if sz_r == (fromIntegral sz_i) 
        then return $ sk arr
        else return $ (Left "parseTTFF - Problem with hGetArray",st0) 

  where
    mkBounds :: Integral a => a -> (Int,Int)
    mkBounds i = (0, fromIntegral $ i - 1)
    
    freezeByteSequence :: IOUArray Int Word8 -> IO ByteSequence
    freezeByteSequence = freeze
    
    sk :: ByteSequence -> (Either String TTFF, TtffParseState)
    sk arr = case runState `flip` st0 $ runParserT readTTFF arr of
                    (Left (ParseError s),st) -> (Left s, st)
                    (Right a,st)             -> (Right a,st)  
                    
    st0 :: TtffParseState
    st0 =  TtffParseState undefined undefined undefined
    
    
-- Hmmm, if all the /logic/ is in this monolithic function, do we need a 
-- state monad?
readTTFF :: TtffParser r TTFF
readTTFF = do 
    (v,nt)  <- readOffsetSubtable
    lift $ modify $ (\s -> s {table_count=nt})
    
    td      <- readTableDirectory nt
    lift $ modify $ (\s -> s {table_locs=td})
    
    -- all reads from now on are tableJumps
    hdr     <- tableJump "head" readHeadTable
    let loc_fmt = index_to_loc_format hdr
    
    ng      <- tableJump "maxp" maxpNumGlyphs
    (_,ge)  <- tableRegion "glyf" 
    
    glocs   <- tableJump "loca" (readLocaTable loc_fmt ng ge)
    lift $ modify $ (\s -> s {glyph_locs=glocs})
    
    -- temporary
    gs      <- tableJump "glyf" (readGlyphs (take 1 glocs))
    return $ TTFF v nt hdr gs



tableJump :: String -> TtffParser r a -> TtffParser r a
tableJump name p = do
    m   <- lift $ gets table_locs 
    maybe fk sk $ tableLocation name m
  where
    fk = reportError $ "parsing failed, missing table '" ++ name ++ "'"    
    sk (offset,len) = withinRange offset len p 
    
tableRegion :: String -> TtffParser r Region
tableRegion name = do
    m   <- lift $ gets table_locs 
    maybe fk return $ tableLocation name m
  where
    fk = reportError $ "parsing failed, missing table '" ++ name ++ "'"    
    
        
--------------------------------------------------------------------------------
-- Offset subtable

-- For the moment the only value we extract is sfnt version and numTables
readOffsetSubtable :: Monad m => ParserT r m (SfntVersion,Int) 
readOffsetSubtable = (\v nt _ _ _ -> (v, fromIntegral nt)) <$>
    sfntVersion <*> ushort <*> ushort <*> ushort <*> ushort


sfntVersion :: Monad m => ParserT r m SfntVersion
sfntVersion = count 4 char >>= fn where 
    fn s | s == ['\0','\1','\0','\0'] = return Sfnt_1_0
         | s == "OTTO"                = return OTTO
         | otherwise                  = err s
        
    err s = reportError $ "unrecognized sfnt string '" ++ s ++ "'"
   




--------------------------------------------------------------------------------
-- Table directory

readTableDirectory :: Monad m => Int -> ParserT r m TableLocs 
readTableDirectory num_tables = do
    ts <- count num_tables tableDescriptor
    return $ buildMap table_name table_location ts
    
    
tableDescriptor :: Monad m => ParserT r m TableDescriptor 
tableDescriptor = 
    (\tag _ s o -> TableDescriptor tag (fromIntegral s, fromIntegral o)) 
        <$> text 4 <*> word32be <*> word32be <*> word32be
        
--------------------------------------------------------------------------------
-- Head table

readHeadTable :: Monad m => ParserT r m FontHeader 
readHeadTable = FontHeader <$>
          fixed 
      <*> fixed 
      <*> ulong 
      <*> ulong
      <*> bitfield ushort       -- head_flags 
      <*> ushort  
      <*> longDateTime
      <*> longDateTime
      <*> readBoundingBox
      <*> bitfield ushort       -- mac_style
      <*> ushort
      <*> short
      <*> short
      <*> short

--------------------------------------------------------------------------------
-- Maxp table

-- Just extract the number of glyphs
 
maxpNumGlyphs :: Monad m => ParserT r m Int 
maxpNumGlyphs = fromIntegral <$> (fixed *> ushort)





--------------------------------------------------------------------------------
-- Loca table

-- External deps 
-- @head@ - idx_to_loc_fmt
-- @maxp@ - num_glyphs
-- table directory - glyph table length & Loca start and length 

-- glyph_table_length is is a synthetic dependency rather
-- than a necessary one, because I interpret the Loca table as I parse it
-- to get the regions of the glyphs in the glyf table 

readLocaTable :: Monad m => 
    Short -> Int -> Int -> ParserT r m [Region]
readLocaTable head_idx_to_loc_fmt maxp_num_glyphs glyf_table_length = do    
        ls    <- glyfLocsFromLocaTable head_idx_to_loc_fmt maxp_num_glyphs
        return $ glyfLocations glyf_table_length ls 

-- Loca table has two formats - one where the data is stored as ushorts
-- the other as ulongs. @indexToLocFormat@ from the @head@ table says 
-- which is used.
glyfLocsFromLocaTable :: Monad m => 
      Short -> Int -> ParserT r m [GlyfStartLoca]
glyfLocsFromLocaTable head_idx_to_loc_fmt maxp_num_glyphs
    | head_idx_to_loc_fmt == 0  = count num_locs primShortFmt
    | otherwise                 = count num_locs primLongFmt
  where
    -- the Loca table includes an index  for the /.notAccess/ glyph
    -- the is not accounted for in the count from the maxp table
    num_locs :: Int
    num_locs        = maxp_num_glyphs + 1 
    
    primLongFmt, primShortFmt :: Monad m => ParserT r m GlyfStartLoca
    primLongFmt     = fromIntegral <$> ulong
     -- short format stores local offset divided by two
    primShortFmt    = (2 *) . fromIntegral <$> ushort



--------------------------------------------------------------------------------
-- Glyf table

readGlyphs :: Monad m => [Region] -> ParserT r m [Glyph]
readGlyphs ((o,l):rs) = readGlyf o l <:> readGlyphs rs
readGlyphs []         = return [] 


readGlyf :: Monad m => Int -> Int -> ParserT r m Glyph
readGlyf offset len = withinRangeRel offset len $ do 
    nc  <- short 
    bb  <- readBoundingBox
    if nc >= 0 
      then do end_pts       <- count (fromIntegral nc) ushort
              let csize     = 1 + fromIntegral (foldr max 0 end_pts)
              _insts        <- countPrefixedList ushort byte
              flags         <- count csize byte
              xy_data       <- runOnL byte
              let outlines  = simpleGlyphContours end_pts flags xy_data
              return $ SimpleGlyph "" bb outlines
              -- don't know the glyph_name at this point
      else 
          reportError $ "Cannot read composite glyphs yet"



readBoundingBox :: Monad m => ParserT r m BoundingBox
readBoundingBox = BoundingBox <$>
    short <*> short <*> short <*> short
       
                             