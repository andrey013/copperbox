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
-- Parse TrueType fonts
-- 
--------------------------------------------------------------------------------

module Graphics.SFont.ParseFont where

import Graphics.SFont.ExtraSyntax
import Graphics.SFont.GlyphDecoder
import Graphics.SFont.KangarooAliases
import Graphics.SFont.PrimitiveDatatypes
import Graphics.SFont.Syntax
import Graphics.SFont.Utils

import Data.ParserCombinators.KangarooState


import Control.Applicative
import Data.Maybe ( catMaybes )
import Data.Monoid




evalParseTTFF :: FilePath -> IO (Either String TTFF)
evalParseTTFF path = fst <$> runParseTTFF path


-- for debugging its handy to be able to view the state

runParseTTFF :: FilePath -> IO (Either String TTFF, TtffParseState)
runParseTTFF = runKangaroo (readTTFF `substError` "READ_FAIL") st0
  where                    
    st0 =  TtffParseState 0 mempty []
    
    
-- Hmmm, if all the /logic/ is in this monolithic function, do we need a 
-- state monad?
readTTFF :: FontParser TTFF
readTTFF = do 
    (v,nt)  <- readOffsetSubtable
    modify $ (\s -> s {table_count=nt})
    
    td      <- readTableDirectory nt
    modify $ (\s -> s {table_locs=td})
    
    -- all reads from now on are tableJumps
    hdr     <- tableJump "head" readHeadTable
    let loc_fmt = index_to_loc_format hdr
    
    ng      <- tableJump "maxp" maxpNumGlyphs
    (_,ge)  <- tableRegion "glyf" 
    
    glocs   <- tableJump "loca" (readLocaTable loc_fmt ng ge)
    modify $ (\s -> s {glyph_locs=glocs})
    

    nrecs   <- tableJump "name" readNameRecords

    glyfs   <- tableJump "glyf" (readGlyphs glocs)
    return $ TTFF v nt hdr nrecs glyfs



tableJump :: String -> FontParser a -> FontParser a
tableJump name p = do
    m   <- gets table_locs 
    maybe fk sk $ tableLocation name m
  where
    fk = reportError $ "parsing failed, missing table '" ++ name ++ "'"    
    sk (offset,len) = withinRegionRel offset len p 
    
tableRegion :: String -> FontParser Region
tableRegion name = do
    m   <- gets table_locs 
    maybe fk return $ tableLocation name m
  where
    fk = reportError $ "parsing failed, missing table '" ++ name ++ "'"    
    
        
--------------------------------------------------------------------------------
-- Offset subtable

-- For the moment the only value we extract is sfnt version and numTables
readOffsetSubtable :: FontParser (SfntVersion,Int) 
readOffsetSubtable = (\v nt _ _ _ -> (v, fromIntegral nt)) <$>
    sfntVersion <*> ushort <*> ushort <*> ushort <*> ushort


sfntVersion :: FontParser SfntVersion
sfntVersion = count 4 char >>= fn where 
    fn s | s == ['\0','\1','\0','\0'] = return SFNT_1_0
         | s == "OTTO"                = return OTTO
         | otherwise                  = err s
        
    err s = reportError $ "unrecognized sfnt string '" ++ s ++ "'"
   




--------------------------------------------------------------------------------
-- Table directory

readTableDirectory :: Int -> FontParser TableLocs 
readTableDirectory num_tables = do
    ts <- count num_tables tableDescriptor
    return $ buildMap table_name table_location ts
    
    
tableDescriptor :: FontParser TableDescriptor 
tableDescriptor = 
    (\tag _ s o -> TableDescriptor tag (fromIntegral s, fromIntegral o)) 
        <$> text 4 <*> word32be <*> word32be <*> word32be
        
--------------------------------------------------------------------------------
-- Head table

readHeadTable :: FontParser FontHeader 
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
 
maxpNumGlyphs :: FontParser Int 
maxpNumGlyphs = fromIntegral <$> (fixed *> ushort)

--------------------------------------------------------------------------------
-- name table



readNameRecords :: FontParser [NameRecord]
readNameRecords = do
    start   <- position
    _fmt    <- ushort
    n       <- ushort
    off     <- ushort
    recs    <- count (fromIntegral n) (readNameRecord $ start + fromIntegral off)
    return recs


readNameRecord :: Int -> FontParser NameRecord 
readNameRecord abs_data_offset = do
    pid     <- platformId  
    enc     <- encodingId 
    lang    <- ushort
    nid     <- nameId
    len     <- ushort
    pos     <- ushort
    str     <- withinRegion (abs_data_offset + fromIntegral pos) 
                            (fromIntegral len)
                            (runOn char)
    return $ NameRecord pid enc lang nid str
    
nameId :: FontParser NameId
nameId = toEnum . fromIntegral <$> ushort





--------------------------------------------------------------------------------
-- Glyf table
readGlyphs :: [Region] -> FontParser [Glyph]
readGlyphs xs = catMaybes <$> readGlyphs' xs

readGlyphs' :: [Region] -> FontParser [Maybe Glyph]
readGlyphs' ((o,l):rs) = readGlyf o l <:> readGlyphs' rs
readGlyphs' []         = return [] 


readGlyf :: Int -> Int -> FontParser (Maybe Glyph)
readGlyf _      0   = return Nothing
readGlyf offset len = withinRegionRel offset len $ do 
    nc  <- short 
    bb  <- readBoundingBox
    if nc >= 0 
      then do 
        end_pts       <- count (fromIntegral nc) ushort
        let csize     = 1 + fromIntegral (foldr max 0 end_pts)
        _insts        <- countPrefixed ushort byte
        flags         <- count csize byte
        xy_data       <- runOn byte
        let outlines  = simpleGlyphContours end_pts flags xy_data
        return $ Just $ SimpleGlyph "" bb outlines
        -- don't know the glyph_name at this point
      else do
          elts        <- compositeElements
          return $ Just $ CompositeGlyph "" bb elts
          

--------------------------------------------------------------------------------
-- Loca table

-- External deps 
-- @head@ - idx_to_loc_fmt
-- @maxp@ - num_glyphs
-- table directory - glyph table length & Loca start and length 

-- glyph_table_length is is a synthetic dependency rather
-- than a necessary one, because I interpret the Loca table as I parse it
-- to get the regions of the glyphs in the glyf table 
--
readLocaTable :: Short -> Int -> Int -> FontParser [Region]
readLocaTable head_idx_to_loc_fmt maxp_num_glyphs glyf_table_length = do    
        ls    <- glyfLocsFromLocaTable head_idx_to_loc_fmt maxp_num_glyphs
        return $ glyfLocations glyf_table_length ls 

-- Loca table has two formats - one where the data is stored as ushorts
-- the other as ulongs. @indexToLocFormat@ from the @head@ table says 
-- which is used.
--
glyfLocsFromLocaTable :: Short -> Int -> FontParser [GlyfStartLoca]
glyfLocsFromLocaTable head_idx_to_loc_fmt maxp_num_glyphs
    | head_idx_to_loc_fmt == 0  = count num_locs primShortFmt
    | otherwise                 = count num_locs primLongFmt
  where
    -- the Loca table includes an index  for the /.notAccess/ glyph
    -- the is not accounted for in the count from the maxp table
    num_locs :: Int
    num_locs        = maxp_num_glyphs + 1 
    
    primLongFmt, primShortFmt :: FontParser GlyfStartLoca
    primLongFmt     = fromIntegral <$> ulong
     -- short format stores local offset divided by two
    primShortFmt    = (2 *) . fromIntegral <$> ushort



--------------------------------------------------------------------------------
-- Common elements



-- The /last/ region only has a start position, so we supply the end loc 
-- of the glyf table (glyf_table_length). This nicely works backwards hence 
-- the right fold.  
glyfLocations :: Int -> [GlyfStartLoca] -> [Region]
glyfLocations glyf_table_length = snd . foldr fn (glyf_table_length,[]) 
  where
    fn i (j,rs) = let i' = fromIntegral i in (i',(i',j-i'):rs)
    


platformId :: FontParser PlatformId 
platformId = toEnum . fromIntegral <$> ushort 
      
encodingId :: FontParser EncodingId 
encodingId = toEnum . fromIntegral <$> ushort 


readBoundingBox :: FontParser BoundingBox
readBoundingBox = BoundingBox <$>
    short <*> short <*> short <*> short
       
                             