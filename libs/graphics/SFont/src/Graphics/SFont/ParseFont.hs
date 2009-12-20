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

-- import Graphics.SFont.GlyphDecoder
import Graphics.SFont.KangarooAliases
import Graphics.SFont.Syntax
import Graphics.SFont.Utils

import Data.ParserCombinators.KangarooWriter


import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Int
import Data.List 
import qualified Data.Map as Map 



evalParseFont :: FilePath -> IO (Either String FontFile)
evalParseFont path = fst <$> runParseFont path 
  


-- for debugging its handy to be able to view the state

runParseFont :: FilePath -> IO (Either String FontFile, Log)
runParseFont = runKangaroo (fontFile `substError` "READ_FAIL") 
    

parseWith :: Parser a -> FilePath -> IO (Either String a, Log)
parseWith p file_name = runKangaroo p file_name
    

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
    glyf_tbl        <- parseTable locs "glyf" glyfTable
    return $ FontFile 
               { ff_offset_table      = offs_tbl
               , ff_head_table        = head_tbl
               , ff_maxp_table        = maxp_tbl
               , ff_loca_table        = loca_tbl
               , ff_name_table        = name_tbl
               , ff_glyf_table        = glyf_tbl
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
offsetTable = OffsetTable <$> sfntVersion <*> uint16
                          <*> uint16      <*> uint16 <*> uint16

sfntVersion :: Parser SfntVersion
sfntVersion = count 4 char >>= fn where 
    fn s | s == ['\0','\1','\0','\0'] = return SFNT_1_0
         | s == "OTTO"                = return OTTO
         | otherwise                  = err s
        
    err s = reportError $ "unrecognized sfnt string '" ++ s ++ "'"
   
-- Table directory

tableDirectory :: Int -> Parser TableLocs 
tableDirectory i = liftM build $ count i tableDescriptor
  where 
    build = buildMap table_name table_location
    
    
tableDescriptor :: Parser TableDescriptor 
tableDescriptor = (\tag _ r -> TableDescriptor tag r)
    <$> text 4 <*> word32be <*> region


--------------------------------------------------------------------------------
-- glyf table

glyfTable :: Parser GlyfTable
glyfTable = GlyfTable <$> count 1 glyf

glyf :: Parser Glyf
glyf = do 
    hdr  <- glyfHeader
    desc <- glyfDescr $ fromIntegral $ glyf_num_contours hdr
    return $ Glyf hdr desc  


glyfHeader :: Parser GlyfHeader
glyfHeader = GlyfHeader <$> int16 <*> fwordBBox

glyfDescr :: Int -> Parser GlyfDescription
glyfDescr i | i > 0     = DescSimple <$> simpleGlyf i
            | otherwise = compositeGlyf

simpleGlyf :: Int -> Parser SimpleGlyf
simpleGlyf n = do 
   end_pts   <- count n uint16
   fc        <- calcFlagCount end_pts  
   instr_len <- uint16
   instrs    <- count (fromIntegral instr_len) uint8
   flags     <- count fc uint8
   (xs,ys)   <- simpleGlyfCoordinates flags
   return $ SimpleGlyf
              { sglyf_end_points      = end_pts
              , sglyf_instr_len       = instr_len
              , sglyf_instructions    = instrs
              , sglyf_flags           = flags
              , sglyf_x_coordinates   = xs
              , sglyf_y_coordinates   = ys
              }


-- | The number of flags appears to be the last contour point (+1)
-- I don\'t know the derivation of this fact - the specs seem to 
-- ellide it, but the clearest font parsing codes found by a
-- Google code search seem to do this.  
calcFlagCount :: [Uint16] -> Parser Int
calcFlagCount [] = reportError "simpleGlyf unreachable!"
calcFlagCount xs = return $ fromIntegral $ 1 + last xs

simpleGlyfCoordinates :: [Uint8] -> Parser ([OutlinePoint],[OutlinePoint])
simpleGlyfCoordinates flags = let (pxs,pys) = buildCoordParser flags in
    sequence pxs >>= \xs -> sequence pys >>= \ys -> return (xs,ys)

-- This is not a fold. If the repeat bit is set the next /flag/ is 
-- actually the number of repeats, so the speed is not one-at-once.
--
buildCoordParser :: [Uint8] -> ([Parser OutlinePoint],[Parser OutlinePoint])
buildCoordParser = step ([],[]) where
    step (xs,ys) []      = (xs,ys)
    step (xs,ys) (f1:fs) = let (repeats,x,y) = interpFlags f1 in
        case repeats of
          False -> step (x:xs,y:ys) fs
          True  -> let (n,fs') = reps fs in
                   step (replicate n x ++ xs, replicate n y ++ ys) fs'

    reps :: [Uint8] -> (Int,[Uint8])
    reps []     = (0,[])
    reps (z:zs) = (fromIntegral z,zs)

-- 0 is LSB
interpFlags :: Uint8 -> (Bool,Parser OutlinePoint,Parser OutlinePoint)
interpFlags u8 = (rep,crM pX,crM pY)
  where
    rep = u8 `testBit` 3 
    pX  = coordType (u8 `testBit` 1) (u8 `testBit` 4)
    pY  = coordType (u8 `testBit` 2) (u8 `testBit` 5)
    crM = if u8 `testBit` 0 then liftM OnCurve else liftM OffCurve
    
coordType :: Bool -> Bool -> Parser DeltaInt16
coordType True  False  = coordUint8 negate
coordType True  True   = coordUint8 id
coordType False True   = coordSame
coordType False False  = coordInt16


-- Consume nothing
coordSame :: Parser DeltaInt16
coordSame = return Same

-- Parse an uint8, promoting it to a (Delta) Int16 
coordUint8 :: (Int16 -> Int16) -> Parser DeltaInt16 
coordUint8 fn = liftM (DInt16 . fn . fromIntegral) uint8

-- Parse an Int16
coordInt16 :: Parser DeltaInt16
coordInt16 = liftM DInt16 int16


compositeGlyf :: Parser GlyfDescription
compositeGlyf = do
    _flags   <- uint16
    _idex    <- uint16
    error $ "composite"


{-

compositeElements :: Parser [CompositeElement]
compositeElements = do 
    (a,more)  <- compositeElt
    if not more then return [a] else (return a) <:> compositeElements
            
            
compositeElt :: Parser (CompositeElement,Bool)  
compositeElt = do 
    flag  <- ushort
    gidx  <- (fromIntegral <$> ushort)
    args  <- extractArgs (argsAreWords flag) (argmaker $ argsAreXYVals flag) 
    op    <- cond3 (haveScale flag)     (Scale   <$> f2dot14) 
                   (haveXYScale flag)   (XyScale <$> f2dot14 <*> f2dot14) 
                   (have2x2 flag)       twoByTwo
                   NoTrans
    return (CompositeElement gidx args op, moreComponents flag)                   
  where
    extractArgs True f = (\a b -> f (fromIntegral a, fromIntegral b)) <$>
                            short <*> short
                          
    extractArgs _    f = (\a b -> f (fromIntegral a, fromIntegral b)) <$>
                            byte <*> byte                          
    
    argmaker True (x,y) = OffsetArgs x y  
    argmaker _    (x,y) = PointNumbers x y 
    
    twoByTwo :: Parser CompositeTrans
    twoByTwo = TwoByTwo <$> f2dot14 <*> f2dot14 <*> f2dot14 <*> f2dot14


extractWordArgs :: Parser (Int,Int)
extractWordArgs = appro (,) fromIntegral fromIntegral <$> short <*> short

extractByteArgs :: Parser (Int,Int)
extractByteArgs = appro (,) fromIntegral fromIntegral <$> byte <*> byte

    
cond3 :: Monad m => Bool -> m a -> Bool -> m a -> Bool -> m a -> a -> m a
cond3 pred1 f1 pred2 f2 pred3 f3 deft
    | pred1     = f1
    | pred2     = f2
    | pred3     = f3
    | otherwise = return deft
  
    

argsAreWords      :: UShort -> Bool
argsAreWords      = testBit `flip` 0

argsAreXYVals     :: UShort -> Bool
argsAreXYVals     = testBit `flip` 1

haveScale         :: UShort -> Bool
haveScale         = testBit `flip` 3

moreComponents    :: UShort -> Bool
moreComponents    = testBit `flip` 5

haveXYScale       :: UShort -> Bool
haveXYScale       = testBit `flip` 6

have2x2           :: UShort -> Bool
have2x2           = testBit `flip` 7
-}

{-
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
-}          
        
--------------------------------------------------------------------------------
-- head table

headTable :: Parser HeadTable
headTable = HeadTable <$>
          fixed 
      <*> fixed 
      <*> uint32
      <*> uint32
      <*> uint16
      <*> uint16  
      <*> longDateTime
      <*> longDateTime
      <*> fwordBBox
      <*> uint16
      <*> uint16
      <*> int16
      <*> locaFormat
      <*> int16
  where
    locaFormat = int16 >>= enumLoca

enumLoca :: Int16 -> Parser LocaFormat
enumLoca 0 = return LocaShort
enumLoca 1 = return LocaLong
enumLoca z = reportError $ "invalid loca format, should be 0 or 1, " ++ show z

--------------------------------------------------------------------------------
-- loca table


locaTable :: LocaFormat -> Int -> Parser LocaTable
locaTable LocaShort n = LocaTable <$> locaShort n
locaTable LocaLong  n = LocaTable <$> locaLong  n

locaShort :: Int -> Parser [Uint32]
locaShort i = count i $ liftM ((2*) . fromIntegral) uint16

locaLong :: Int -> Parser [Uint32]
locaLong = count `flip` uint32

--------------------------------------------------------------------------------
-- maxp table

 
maxpTable :: Parser MaxpTable 
maxpTable = MaxpTable <$> fixed  <*> uint16 <*> uint16 <*> uint16
                      <*> uint16 <*> uint16 <*> uint16 <*> uint16
                      <*> uint16 <*> uint16 <*> uint16 <*> uint16
                      <*> uint16 <*> uint16 <*> uint16

--------------------------------------------------------------------------------
-- name table

nameTable :: Parser NameTable
nameTable = do 
    pos         <- currentParsePosition
    fmt         <- uint16
    tot         <- uint16
    str_off     <- uint16
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
    pid     <- uint16  
    enc     <- uint16
    lang    <- uint16
    nid     <- uint16
    len     <- uint16
    pos     <- uint16
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



--------------------------------------------------------------------------------
-- Common elements



-- The /last/ region only has a start position, so we supply the end loc 
-- of the glyf table (glyf_table_length). This nicely works backwards hence 
-- the right fold.  
--
glyfLocations :: Int -> [Uint32] -> [Region]
glyfLocations = snd `oo` mapAccumR fn where
    fn acc x = (x', Region x' (acc-x')) where x' = fromIntegral x



fwordBBox :: Parser FWordBBox
fwordBBox = BoundingBox <$> fword <*> fword <*> fword <*> fword
       
region :: Parser Region 
region = Region <$> w32i <*> w32i
  where w32i = liftM fromIntegral word32be