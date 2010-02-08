{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZFont.ParseFont
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Parse TrueType fonts
-- 
--------------------------------------------------------------------------------

module Graphics.ZFont.ParseFont where

import Graphics.ZFont.KangarooAliases
import Graphics.ZFont.Syntax
import Graphics.ZFont.Utils

import Data.ParserCombinators.KangarooWriter


import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Int
import Data.List 
import qualified Data.Map as Map 



evalParseFont :: FilePath -> IO (Either String FontFile)
evalParseFont path = fst <$> runParseFont path 
  



runParseFont :: FilePath -> IO (Either String FontFile, Log)
runParseFont = runKangaroo fontFile
    

parseWith :: Parser a -> FilePath -> IO (Either String a, Log)
parseWith p file_name = runKangaroo p file_name
    

fontFile :: Parser FontFile
fontFile = do
    (offs_tbl,locs) <- prolog
    head_tbl        <- parseTable locs "head" headTable
    cmap_tbl        <- parseTable locs "cmap" cmapTable
    maxp_tbl        <- parseTable locs "maxp" maxpTable
    loca_tbl        <- parseTable locs "loca" $ 
                         locaTable (ht_index_to_loc_format head_tbl)
                                   (fromIntegral $ maxp_num_glyphs maxp_tbl)
    name_tbl        <- parseTable locs "name" nameTable
    glyf_tbl        <- parseTable locs "glyf" glyfTable
    post_tbl        <- parseTable locs "post" postTable
    return $ FontFile 
               { ff_offset_table      = offs_tbl
               , ff_head_table        = head_tbl
               , ff_cmap_table        = cmap_tbl
               , ff_maxp_table        = maxp_tbl
               , ff_loca_table        = loca_tbl
               , ff_name_table        = name_tbl
               , ff_glyf_table        = glyf_tbl
               , ff_post_table        = post_tbl
               }


prolog :: Parser (OffsetTable,TableLocs)
prolog = mprogress (,) fn offsetTable tableDirectory
  where
    fn = fromIntegral . ot_number_of_tables

-- needs renaming...
mprogress :: Monad m => (a -> c -> d) -> (a -> b) -> m a -> (b -> m c) -> m d
mprogress comb f ma mb = ma >>= \a -> mb (f a) >>= \b -> return $ comb a b

-- Note this one shouldn't matter where the parsing finishes
--
parseTable :: TableLocs -> String -> Parser a -> Parser a
parseTable mp name p = 
    getRegion >>= \(Region start len) -> intraparse name Dalpunto start len p
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
sfntVersion = count 4 anyChar >>= fn where 
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
    <$> text 4 <*> word32be <*> tableregion


--------------------------------------------------------------------------------
-- cmap table

cmapTable :: Parser CmapTable
cmapTable = do 
    start_pos   <- currentParsePosition
    ix          <- cmapIndex
    sub_headers <- count (fromIntegral $ cmap_num_subtables ix) 
                         cmapSubtableHeader
    sub_bodies  <- mapM (fn start_pos . cmap_offset) sub_headers
    return $ CmapTable 
              { cmap_index          = ix
              , cmap_subt_headers   = sub_headers
              , cmap_subt_bodies    = sub_bodies
              }
  where
    fn :: Int -> Uint32 -> Parser CmapSubtableBody
    fn table_start offset = let pos = table_start + fromIntegral offset in
        advance "cmap-subtable" Alfermata pos cmapSubtableBody
 

cmapIndex :: Parser CmapIndex
cmapIndex = CmapIndex <$> uint16 <*> uint16


cmapSubtableHeader :: Parser CmapSubtableHeader
cmapSubtableHeader = CmapSubtableHeader <$> uint16 <*> uint16 <*> uint32

cmapSubtableBody :: Parser CmapSubtableBody
cmapSubtableBody = uint16 >>= subtable
  where
    subtable 0  = liftM CmapFmt0  cmapFormat0
    subtable 2  = liftM CmapFmt2  cmapFormat2
    subtable 4  = liftM CmapFmt4  cmapFormat4
    subtable 6  = liftM CmapFmt6  cmapFormat6
    subtable 8  = liftM CmapFmt8  (uint16 >> cmapFormat8)
    subtable 10 = liftM CmapFmt10 (uint16 >> cmapFormat10)
    subtable 12 = liftM CmapFmt12 (uint16 >> cmapFormat12)
    subtable n  = reportError $ "unrecognized cmap subtable " ++ show n

cmapFormat0 :: Parser CmapFormat0
cmapFormat0 = CmapFormat0 <$> uint16 <*> uint16 <*> count 256 uint8


cmapFormat2 :: Parser CmapFormat2
cmapFormat2 = reportError "cmap format2 subtables not supported"

format2_Subheader :: Parser Format2_Subheader
format2_Subheader = Format2_SubHeader <$> uint16 <*> uint16 
                                      <*> int16  <*> uint16

subtableRestrict :: Int -> Uint16 -> Parser a -> Parser a
subtableRestrict fmt n p = do
    liftIOAction $ putStrLn $  "subtableRestrict " ++ show (n - subheader_len)
    restrict "cmap-subtable rest" Alfermata (fromIntegral $ n - subheader_len) p
  where
    subheader_len = if elem fmt [8,10,12] then fmt_resv_len else fmt_plus_len
    fmt_plus_len  = 4
    fmt_resv_len  = 6

cmapFormat4 :: Parser CmapFormat4
cmapFormat4 = uint16 >>= \len -> subtableRestrict 4 len $ do
    lang            <- uint16
    seg_count_x2    <- uint16
    let seg_count   = fromIntegral $ seg_count_x2 `div` 2
    search_range    <- uint16
    entry_sel       <- uint16
    range_shift     <- uint16 
    end_codes       <- count seg_count uint16
    resv_pad        <- uint16
    start_codes     <- count seg_count uint16
    id_deltas       <- count seg_count uint16
    id_ranges       <- count seg_count uint16
    glyf_indexes    <- runOn uint16
    return $ CmapFormat4
              { fmt4_length           = len
              , fmt4_lang_code        = lang
              , fmt4_segcount_x2      = seg_count_x2
              , fmt4_search_range     = search_range
              , fmt4_entry_selector   = entry_sel
              , fmt4_range_shift      = range_shift
              , fmt4_end_code         = end_codes
              , fmt4_reserved_pad     = resv_pad
              , fmt4_start_code       = start_codes
              , fmt4_id_delta         = id_deltas
              , fmt4_id_range_offset  = id_ranges
              , fmt4_glyf_idxs        = glyf_indexes
              }
cmapFormat6 :: Parser CmapFormat6
cmapFormat6 = reportError "cmap format6 subtables not supported"

cmapFormat8 :: Parser CmapFormat8
cmapFormat8 = reportError "cmap format8 subtables not supported"

cmapFormat10 :: Parser CmapFormat10
cmapFormat10 = reportError "cmap format10 subtables not supported"

cmapFormat12 :: Parser CmapFormat12
cmapFormat12 = do 
    len   <- uint32
    lang  <- uint32
    (n,grps)  <- countPrefixed uint32 cmapGroup
    return $ CmapFormat12
              { fmt12_length        = len
              , fmt12_lang_code     = lang
              , fmt12_num_groups    = n
              , fmt12_groups        = grps
              }
cmapGroup :: Parser CmapGroup
cmapGroup = CmapGroup <$> uint32 <*> uint32 <*> uint32


--------------------------------------------------------------------------------
-- glyf table

glyfTable :: Parser GlyfTable
glyfTable = GlyfTable <$> count 1 glyf

glyf :: Parser Glyf
glyf = do 
    header  <- glyfHeader
    desc    <- glyfDescr $ fromIntegral $ glyf_num_contours header
    return $ Glyf header desc  


glyfHeader :: Parser GlyfHeader
glyfHeader = GlyfHeader <$> int16 <*> fwordBBox

glyfDescr :: Int -> Parser GlyfDescription
glyfDescr i | i > 0     = DescSimple   <$> simpleGlyf i
            | otherwise = DescCompound <$> compoundGlyf

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


-- Compound glyfs


compoundGlyf :: Parser CompoundGlyf
compoundGlyf = do 
    (cs,has_instr) <- componentGlyfs
    instrs         <- if has_instr then compoundGlyfInstrs else return [] 
    return $ CompoundGlyf
              { cglyf_components    = cs
              , cglyf_instructions  = instrs
              }

componentGlyfs :: Parser ([ComponentGlyf],Bool)
componentGlyfs = 
    liftM finish $ genericManyTillPC comb ([],Nothing) componentGlyf check
  where
    comb a (xs,Nothing) = (a:xs, Just $ (`testBit` 8) $ cglyf_flags a)
    comb a (xs,z)       = (a:xs,z)
    
    check               = (`testBit` 5) . cglyf_flags

    finish ((xs,z),x)   = (xs++[x], maybe False id z)

componentGlyf :: Parser ComponentGlyf
componentGlyf = do 
    flags       <- uint16
    idx         <- uint16
    (arg1,arg2) <- offsetsOrIndexes (arg_1_and_2_are_words flags)
                                    (args_are_xy_values    flags)
    mtrx        <- linearMatrix (we_have_a_scale      flags)
                                (we_have_an_xy_scale  flags)
                                (we_have_a_two_by_two flags)  
    return $ ComponentGlyf 
              { cglyf_flags         = flags
              , cglyf_index         = idx
              , cglyf_argument1     = arg1
              , cglyf_argument2     = arg2
              , cglyf_trans         = mtrx
              }

arg_1_and_2_are_words   :: Uint16 -> Bool
arg_1_and_2_are_words   = testBit `flip` 0

args_are_xy_values      :: Uint16 -> Bool
args_are_xy_values      = testBit `flip` 1

we_have_a_scale         :: Uint16 -> Bool
we_have_a_scale         = testBit `flip` 3

we_have_an_xy_scale     :: Uint16 -> Bool
we_have_an_xy_scale     = testBit `flip` 6

we_have_a_two_by_two    :: Uint16 -> Bool
we_have_a_two_by_two    = testBit `flip` 3



compoundGlyfInstrs :: Parser [Uint8]
compoundGlyfInstrs = liftM snd $ countPrefixed uint16 uint8

offsetsOrIndexes :: Bool -> Bool -> Parser (OffsetOrIndex, OffsetOrIndex)
offsetsOrIndexes True  True  = (,) <$> shortValue <*> shortValue
offsetsOrIndexes False True  = (,) <$> byteValue  <*> byteValue
offsetsOrIndexes True  False = (,) <$> shortIndex <*> shortIndex
offsetsOrIndexes False False = (,) <$> byteIndex  <*> byteIndex

shortValue :: Parser OffsetOrIndex
shortValue = CG_Offset <$> int16 

byteValue :: Parser OffsetOrIndex
byteValue = CG_Offset . fromIntegral <$> uint8

shortIndex :: Parser OffsetOrIndex
shortIndex = CG_Index <$> int16

byteIndex :: Parser OffsetOrIndex
byteIndex = CG_Index . fromIntegral <$> int16


linearMatrix :: Bool -> Bool -> Bool -> Parser Matrix4

linearMatrix False False False =        -- No transform
    Matrix4 <$> c1Dot14 1.0 <*> c1Dot14 0.0 <*> c1Dot14 0.0 <*> c1Dot14 1.0

linearMatrix True  False False =        -- Uniform scale
    (\a b ->  Matrix4 a b b a) <$> s1Dot14 <*> c1Dot14 0.0
                                         
linearMatrix False True  False =        -- scale
    Matrix4 <$> s1Dot14 <*> c1Dot14 0.0 <*> c1Dot14 0.0 <*> s1Dot14

linearMatrix False False True  =        -- TWO_BY_TWO
    Matrix4 <$> s1Dot14 <*> s1Dot14 <*> s1Dot14 <*> s1Dot14

linearMatrix x     y     z     =
    reportError $ "composite glyph - invalid trafo flags " ++ show (x,y,z)


c1Dot14 :: Double -> Parser OneDot14 
c1Dot14 = return . OneDot14_Const 

s1Dot14 :: Parser OneDot14
s1Dot14 = liftM OneDot14_Int16 int16


        
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
    let str_loc = str_data_offset + fromIntegral pos
    str     <- intraparse "name-record" Dalpunto           str_loc 
                                        (fromIntegral len) (runOn anyChar)
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
-- post table

postTable :: Parser PostTable
postTable = PostTable <$> fixed <*> fixed



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
       
tableregion :: Parser Region 
tableregion = Region <$> w32i <*> w32i
  where w32i = liftM fromIntegral word32be


