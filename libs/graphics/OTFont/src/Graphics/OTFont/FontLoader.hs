{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.FontLoader
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Cache loaded tables
--
--------------------------------------------------------------------------------


module Graphics.OTFont.FontLoader where

import Graphics.OTFont.Datatypes

import Graphics.OTFont.ParseMonad
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras
import Graphics.OTFont.Pretty

import Graphics.OTFont.Table.Cmap ( readCmapTable )
import Graphics.OTFont.Table.Head ( HeadTable(..), readHeadTable )
import Graphics.OTFont.Table.Hhea ( HheaTable(..), readHheaTable )
import Graphics.OTFont.Table.Hmtx ( readHmtxTable )
import Graphics.OTFont.Table.Loca ( readLocaTable )
import Graphics.OTFont.Table.Maxp ( MaxpTable(..), readMaxpTable )
import Graphics.OTFont.Table.Name ( readNameTable )
import Graphics.OTFont.Table.OS2  ( readOS2Table  )
import Graphics.OTFont.Table.Post ( readPostTable )


import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Data.Array.IO
import Data.Dynamic
import qualified Data.Map as Map
import Data.Word
import System.IO 

import Text.PrettyPrint.Leijen ( Pretty(..), string )



data FontLoaderState = FontLoaderState { 
      path_to_font    :: FilePath,
      offset_table    :: OffsetTable,
      table_offsets   :: TableDirectory,
      table_cache     :: TableCache
    }

type TableDirectory = Map.Map String Region
type TableCache     = Map.Map String Dynamic

  
data OffsetTable = OffsetTable {
      sfnt_version    :: String,
      num_tables      :: Int,
      search_range    :: UShort,
      entry_selector  :: UShort,
      range_shift     :: UShort
    } 
  deriving (Eq,Show)      
      
data TableDescriptor = TableDescriptor {
      table_tag       :: String,
      table_location  :: Region
    } 
  deriving (Eq,Show)  


-- FontLoader is a State Monad transformed by ParserT   
type FontLoader r a = ParserT r (State FontLoaderState) a



runFontLoader :: FilePath -> FontLoader r r -> IO (Either String r)
runFontLoader path p = withBinaryFile path ReadMode $ \h -> do
    sz_i  <- hFileSize h
    let abound = mkBounds sz_i
    marr   <- newArray_ abound 
    sz_r  <- hGetArray h marr (fromIntegral sz_i)
    arr   <- freezeByteSequence marr
    if sz_r == (fromIntegral sz_i) 
        then return $ runFL p arr
        else error $ "Problem with hGetArray" 

  where
    mkBounds :: Integral a => a -> (Int,Int)
    mkBounds i = (0, fromIntegral $ i - 1)
    
    freezeByteSequence :: IOUArray Int Word8 -> IO ByteSequence
    freezeByteSequence = freeze
    
    runFL :: FontLoader r r -> ByteSequence -> Either String r
    runFL m arr = case evalState `flip` st0 $ runParserT (adapt m) arr of
                    Left (ParseError s) -> Left s
                    Right a             -> Right a  
    
    -- Initial some fields of FontLoaderState have to be undefined
    st0 :: FontLoaderState
    st0 = FontLoaderState { path_to_font    = path
                          , offset_table    = undefined
                          , table_offsets   = Map.empty
                          , table_cache     = Map.empty
                          }
    
    adapt :: FontLoader r r -> FontLoader r r
    adapt fn = do 
        o_t       <- offsetTable
        ds        <- count (num_tables o_t) tableDescriptor
        let t_os  = mkTableDirectory ds
        lift $ modify (\s -> s { offset_table = o_t, table_offsets = t_os })
        fn 
        



tableFromCache :: String -> FontLoader r (Maybe Dynamic)
tableFromCache name = do 
    cache <- lift (gets table_cache) 
    return $ Map.lookup name cache 

queryTable :: Typeable a => String -> (a -> b) -> String -> FontLoader r b     
queryTable name fn failure_msg = tableFromCache name >>= maybe fk1 sk1 where
    fk1     = throwError $ strMsg $ failure_msg
    sk1 a   = maybe fk2 (return . fn) (fromDynamic a)
    fk2     = throwError $ strMsg $ "fromDynamic failure on '" ++ name ++ "'"

         

 
findTableOffset :: String -> FontLoader r (Maybe Region)
findTableOffset name = Map.lookup name <$> lift (gets table_offsets)
          
--------------------------------------------------------------------------------    
  
     
offsetTable :: FontLoader r OffsetTable 
offsetTable = OffsetTable
    <$> prefix <*> intAsUShort <*> ushort <*> ushort <*> ushort
  where
    prefix :: FontLoader r String
    prefix = satisfies (count 4 char) (\s -> s == "OTTO" || s == o1oo)
    
    o1oo :: String
    o1oo = ['\0','\1','\0','\0']
    
    intAsUShort :: FontLoader r Int
    intAsUShort  = fromIntegral <$> ushort
    
tableDescriptor :: FontLoader r TableDescriptor 
tableDescriptor = 
    (\name _ s e -> TableDescriptor name (fromIntegral s, fromIntegral e)) 
        <$> text 4 <*> word32be <*> word32be <*> word32be

mkTableDirectory :: [TableDescriptor] -> TableDirectory
mkTableDirectory ts = foldr fn Map.empty ts
  where
    fn (TableDescriptor n v) fm = Map.insert n v fm

--------------------------------------------------------------------------------
-- load table

loadTable :: String -> FontLoader r ()
loadTable name = tableFromCache name >>= maybe (load name) (const $ return ()) 
  where
    load :: String -> FontLoader r ()
    load "cmap"   = findTableOffset "cmap"  >>=
                    maybe (tableLoadError "cmap") (load2 . readCmapTable)
    load "head"   = load2 readHeadTable
    load "hhea"   = load2 readHheaTable
    
    load "htmx"   = let err1 = "load 'hhea' table before 'hmtx'"
                        err2 = "load 'maxp' table before 'hmtx'"
                    in do i <- queryTable "hhea" number_of_h_metrics err1
                          j <- queryTable "maxp" num_glyphs          err2
                          load2 $ readHmtxTable i j
                          
    load "loca"   = let err1  = "load 'head' table before 'loca'"
                        err2  = "load 'maxp' table before 'loca'"
                    in do i <- queryTable "head" index_to_loc_format err1
                          j <- queryTable "maxp" num_glyphs          err2
                          load2 $ readLocaTable i j        
    
    load "maxp"   = load2 readMaxpTable
    load "name"   = findTableOffset "name"  >>=
                    maybe (tableLoadError "name") (load2 . readNameTable) 
                    
    load "os/2"   = load2 readOS2Table
    load "post"   = load2 readPostTable
                    
    load _        = tableLoadError name
                        
    load2 :: Typeable a =>  FontLoader r a -> FontLoader r ()
    load2 p =  findTableOffset name >>= maybe (tableLoadError name) sk where
        sk (i,j)  = do setRange i j 
                       t <- p
                       cache <- lift $ gets table_cache
                       lift $ modify $ \s -> 
                          s { table_cache = Map.insert name (toDyn t) cache }

decodeTable :: Typeable a => String -> FontLoader r (Maybe a)               
decodeTable name = do 
    loadTable name
    t <- tableFromCache name
    maybe (tableLoadError name) (return . fromDynamic) t
       

tableLoadError :: String -> FontLoader r e
tableLoadError name =  throwError $ strMsg $ 
    "Error - could not load table '" ++ name ++ "'"
     
--------------------------------------------------------------------------------
--    
    
instance Pretty FontLoaderState where
  pretty t = ppTable "Font Loader"  
      [ field "path"   10   (string $ path_to_font t) 
      , pretty $ offset_table t 
      -- , field "map size" 16 (integral (Map.size fm))
      ]
      
      
instance Pretty OffsetTable where
  pretty (OffsetTable s nt sr es rs) = ppTable "Offset Table"  
      [ field "sfnt_version"    16 (pptag s)
      , field "num_tables"      16 (integral nt)
      , field "search_range"    16 (integral sr)
      , field "entry_selector"  16 (integral es)
      , field "range_shift"     16 (integral rs)
      ]

{-
instance Pretty TableDirectory where
  pretty (TableDirectory t cs o tl) = ppTable "Table Directory" 
      [ field "tag"             16 (pptag t)
      , field "offset"          16 (integral o)
      , field "table_length"    16 (integral tl)
      ]
-}      