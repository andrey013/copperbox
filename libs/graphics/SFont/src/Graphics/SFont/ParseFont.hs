{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.ParseFont
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parse TrueType fonts
-- 
--------------------------------------------------------------------------------

module Graphics.SFont.ParseFont where

import Graphics.SFont.GlyphDecoder
import Graphics.SFont.KangarooAliases
import Graphics.SFont.Syntax
import Graphics.SFont.Utils

import Data.ParserCombinators.KangarooWriter


import Control.Applicative
import Control.Monad
import Data.List 
import Data.Maybe ( catMaybes )



evalParseTTFF :: FilePath -> IO (Either String TTFF)
evalParseTTFF path = fst <$> runParseTTFF path 
  


-- for debugging its handy to be able to view the state

runParseTTFF :: FilePath -> IO (Either String TTFF, Log)
runParseTTFF = runKangaroo (readTTFF `substError` "READ_FAIL") 
    

parseWithState :: Parser a -> FilePath -> IO (Either String a, Log)
parseWithState p file_name = runKangaroo p file_name
    
    
-- Hmmm, if all the /logic/ is in this monolithic function, do we need a 
-- state monad?
readTTFF :: Parser TTFF
readTTFF = undefined

{-
readTTFF = do 
    (v,nt)  <- offsetTable
    modify $ (\s -> s {table_count=nt})
    
    td      <- readTableDirectory nt
    modify $ (\s -> s {table_locs=td})
    
    -- all reads from now on are tableJumps
    hdr     <- tableJump "head" headTable
    let loc_fmt = index_to_loc_format hdr
    
    ng      <- tableJump "maxp" maxpNumGlyphs
    (_,ge)  <- tableRegion "glyf" 
    
    glocs   <- undefined -- tableJump "loca" (locaTable loc_fmt ng)
    modify $ (\s -> s {glyph_locs=glocs})
    

    nrecs   <- tableJump "name" readNameRecords

    glyfs   <- tableJump "glyf" (readGlyphs glocs)
    return $ TTFF v nt hdr nrecs glyfs



tableJump :: String -> Parser a -> Parser a
tableJump name p = do
    m   <- gets table_locs 
    maybe fk sk $ tableLocation name m
  where
    fk = reportError $ "parsing failed, missing table '" ++ name ++ "'"    
    sk (offset,len) = interversoRel offset len p 
    
tableRegion :: String -> Parser Region
tableRegion name = do
    m   <- gets table_locs 
    maybe fk return $ tableLocation name m
  where
    fk = reportError $ "parsing failed, missing table '" ++ name ++ "'"    
-}    

prolog :: Parser (OffsetTable,TableLocs)
prolog = mprogress (,) number_of_tables offsetTable tableDirectory


        
--------------------------------------------------------------------------------
-- Offset subtable

-- For the moment the only value we extract is sfnt version and numTables
--
offsetTable :: Parser OffsetTable
offsetTable = OffsetTable <$> sfntVersion <*> ushorti 
                          <*> ushorti     <*> ushorti <*> ushorti
  where
    ushorti = liftM fromIntegral ushort

sfntVersion :: Parser SfntVersion
sfntVersion = count 4 char >>= fn where 
    fn s | s == ['\0','\1','\0','\0'] = return SFNT_1_0
         | s == "OTTO"                = return OTTO
         | otherwise                  = err s
        
    err s = reportError $ "unrecognized sfnt string '" ++ s ++ "'"
   




--------------------------------------------------------------------------------
-- Table directory

tableDirectory :: Int -> Parser TableLocs 
tableDirectory i = liftM build $ count i tableDescriptor
  where 
    build = buildMap table_name table_location
    
    
tableDescriptor :: Parser TableDescriptor 
tableDescriptor = 
    (\tag _ s o -> TableDescriptor tag (Region (fromIntegral s) (fromIntegral o)) )
        <$> text 4 <*> word32be <*> word32be <*> word32be
        
--------------------------------------------------------------------------------
-- Head table

headTable :: Parser FontHeader 
headTable = FontHeader <$>
          fixed 
      <*> fixed 
      <*> ulong 
      <*> ulong
      <*> bitfield ushort       -- head_flags 
      <*> ushort  
      <*> longDateTime
      <*> longDateTime
      <*> boundingBox
      <*> bitfield ushort       -- mac_style
      <*> ushort
      <*> short
      <*> locaFormat
      <*> short
  where
    locaFormat = short >>= enumLoca

enumLoca :: Short -> Parser LocaFormat
enumLoca 0 = return LocaShort
enumLoca 1 = return LocaLong
enumLoca z = reportError $ "invalid loca format, should be 0 or 1, " ++ show z

--------------------------------------------------------------------------------
-- Maxp table

-- Just extract the number of glyphs
 
maxpNumGlyphs :: Parser Int 
maxpNumGlyphs = fromIntegral <$> (fixed *> ushort)

--------------------------------------------------------------------------------
-- name table



readNameRecords :: Parser [NameRecord]
readNameRecords = do
    start   <- position
    _fmt    <- ushort
    n       <- ushort
    off     <- ushort
    recs    <- count (fromIntegral n) (readNameRecord $ start + fromIntegral off)
    return recs


readNameRecord :: Int -> Parser NameRecord 
readNameRecord abs_data_offset = do
    pid     <- platformId  
    enc     <- encodingId 
    lang    <- ushort
    nid     <- nameId
    len     <- ushort
    pos     <- ushort
    str     <- interrecto (abs_data_offset + fromIntegral pos) 
                          (fromIntegral len)
                          (runOn char)
    return $ NameRecord pid enc lang nid str
    
nameId :: Parser NameId
nameId = toEnum . fromIntegral <$> ushort





--------------------------------------------------------------------------------
-- Glyf table
readGlyphs :: [Region] -> Parser [Glyph]
readGlyphs xs = catMaybes <$> readGlyphs' xs

readGlyphs' :: [Region] -> Parser [Maybe Glyph]
readGlyphs' (Region o l:rs) = readGlyf o l <:> readGlyphs' rs
readGlyphs' []              = return [] 


readGlyf :: Int -> Int -> Parser (Maybe Glyph)
readGlyf _      0   = return Nothing
readGlyf offset len = interrectoRel offset len $ do 
    nc  <- short 
    bb  <- boundingBox
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
readLocaTable :: Short -> Int -> Int -> Parser [Region]
readLocaTable head_idx_to_loc_fmt maxp_num_glyphs glyf_table_length = do    
        ls    <- glyfLocsFromLocaTable head_idx_to_loc_fmt maxp_num_glyphs
        return $ glyfLocations glyf_table_length ls 

-- Loca table has two formats - one where the data is stored as ushorts
-- the other as ulongs. @indexToLocFormat@ from the @head@ table says 
-- which is used.
--
glyfLocsFromLocaTable :: Short -> Int -> Parser [ULong]
glyfLocsFromLocaTable head_idx_to_loc_fmt maxp_num_glyphs
    | head_idx_to_loc_fmt == 0  = count num_locs primShortFmt
    | otherwise                 = count num_locs primLongFmt
  where
    -- the Loca table includes an index  for the /.notAccess/ glyph
    -- the is not accounted for in the count from the maxp table
    num_locs :: Int
    num_locs        = maxp_num_glyphs + 1 
    
    primLongFmt, primShortFmt :: Parser ULong
    primLongFmt     = fromIntegral <$> ulong
     -- short format stores local offset divided by two
    primShortFmt    = (2 *) . fromIntegral <$> ushort

locaTable :: LocaFormat -> Int -> Parser [ULong]
locaTable LocaShort = locaShort
locaTable LocaLong  = locaLong 

locaShort :: Int -> Parser [ULong]
locaShort i = count i $ liftM ((2*) . fromIntegral) ushort  

locaLong :: Int -> Parser [ULong]
locaLong = count `flip` ulong

--------------------------------------------------------------------------------
-- Common elements



-- The /last/ region only has a start position, so we supply the end loc 
-- of the glyf table (glyf_table_length). This nicely works backwards hence 
-- the right fold.  
--
glyfLocations :: Int -> [ULong] -> [Region]
glyfLocations = snd `oo` mapAccumR fn where
    fn acc x = (x', Region x' (acc-x')) where x' = fromIntegral x


platformId :: Parser PlatformId 
platformId = toEnum . fromIntegral <$> ushort 
      
encodingId :: Parser EncodingId 
encodingId = toEnum . fromIntegral <$> ushort 


boundingBox :: Parser BoundingBox
boundingBox = BoundingBox <$> short <*> short <*> short <*> short
       
                             