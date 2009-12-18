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
import qualified Data.Map as Map 
import Data.Maybe ( catMaybes )



evalParseFont :: FilePath -> IO (Either String FontFile)
evalParseFont path = fst <$> runParseFont path 
  


-- for debugging its handy to be able to view the state

runParseFont :: FilePath -> IO (Either String FontFile, Log)
runParseFont = runKangaroo (fontFile `substError` "READ_FAIL") 
    

parseWith :: Parser a -> FilePath -> IO (Either String a, Log)
parseWith p file_name = runKangaroo p file_name
    

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

-}    

fontFile :: Parser FontFile
fontFile = do
    (offs_tbl,locs) <- prolog
    logline $ show locs
    head_tbl        <- parseTable locs "head" headTable
    maxp_tbl        <- parseTable locs "maxp" maxpTable
    loca_tbl        <- parseTable locs "loca" $ 
                         locaTable (ht_index_to_loc_format head_tbl)
                                   (fromIntegral $ maxp_num_glyphs maxp_tbl)
    name_tbl        <- parseTable locs "name" nameTable
    return $ FontFile 
               { ff_offset_table      = offs_tbl
               , ff_head_table        = head_tbl
               , ff_maxp_table        = maxp_tbl
               , ff_loca_table        = loca_tbl
               , ff_name_table        = name_tbl
               }


prolog :: Parser (OffsetTable,TableLocs)
prolog = mprogress (,) fn offsetTable tableDirectory
  where
    fn = fromIntegral . ot_number_of_tables

parseTable :: TableLocs -> String -> Parser a -> Parser a
parseTable mp name p = 
    getRegion >>= \(Region start len) -> interverso start (start+len) p
  where
    getRegion = maybe errk return $ Map.lookup name mp
    errk      = reportError $ "missing table " ++ name

        
--------------------------------------------------------------------------------
-- Offset subtable

-- For the moment the only value we extract is sfnt version and numTables
--
offsetTable :: Parser OffsetTable
offsetTable = OffsetTable <$> sfntVersion <*> ushort
                          <*> ushort      <*> ushort <*> ushort

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
tableDescriptor = (\tag _ r -> TableDescriptor tag r)
    <$> text 4 <*> word32be <*> region
        
--------------------------------------------------------------------------------
-- head table

headTable :: Parser HeadTable
headTable = HeadTable <$>
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
-- loca table


locaTable :: LocaFormat -> Int -> Parser LocaTable
locaTable LocaShort n = LocaTable <$> locaShort n
locaTable LocaLong  n = LocaTable <$> locaLong  n

locaShort :: Int -> Parser [ULong]
locaShort i = count i $ liftM ((2*) . fromIntegral) ushort  

locaLong :: Int -> Parser [ULong]
locaLong = count `flip` ulong

--------------------------------------------------------------------------------
-- maxp table

-- Just extract the number of glyphs
 
maxpTable :: Parser MaxpTable 
maxpTable = MaxpTable <$> fixed <*> ushort

--------------------------------------------------------------------------------
-- name table

nameTable :: Parser NameTable
nameTable = do 
    pos         <- position
    fmt         <- ushort
    tot         <- ushort
    str_off     <- ushort
    let str_loc = pos + fromIntegral str_off
    names       <- count (fromIntegral tot) (nameRecord str_loc)
    return $ NameTable 
                { nt_format         = fmt
                , nt_count          = tot
                , nt_string_offset  = str_off
                , nt_name_records   = names
                }


nameRecord :: Int -> Parser NameRecord 
nameRecord str_data_offset = do
    pid     <- platformId  
    enc     <- encodingId 
    lang    <- ushort
    nid     <- nameId
    len     <- ushort
    pos     <- ushort
    logline $ show (str_data_offset,len,pos)
    let str_loc = str_data_offset + fromIntegral pos
    str     <- interverso str_loc (str_loc + fromIntegral len) (runOn char)
    return $ NameRecord 
                { nr_platform_id    = pid
                , nr_encoding_id    = enc
                , nr_language_id    = lang
                , nr_name_id        = nid
                , nr_length         = len
                , nr_offset         = pos
                , nr_name_text      = str
                }

    
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
       
region :: Parser Region 
region = Region <$> w32i <*> w32i
  where w32i = liftM fromIntegral word32be