
--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Diff.DiffKit.Printer
-- Copyright   :  (c) Stephen Tetley 2007
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  portable
--
-- Print diffs

--------------------------------------------------------------------------------

module Text.Diff.DiffKit.Printer
 where

import Text.Diff.DiffKit.Datatypes

import Numeric
import System.Time



-- Lines need rendering exactly, so that stops a 
-- pretty-print library being used.


showUnified a = uprint a []

class UnifiedPrint a where uprint :: a -> ShowS


instance UnifiedPrint DiffFile where
  uprint (DiffFile (Just fhdr) hs) = uprint fhdr . nl . uprintSep nl hs . nl

instance UnifiedPrint FileHeader where
  uprint (FileHeader from to)
    = str "--- " . uprint from . nl . str "+++ " . uprint to

instance UnifiedPrint FileStats where
  uprint (FileStats name stamp) = str name . tab . uprint stamp 

uprintSep :: (UnifiedPrint a) => ShowS -> [a] -> ShowS
uprintSep sep []     = showString ""
uprintSep sep [x]    = uprint x
uprintSep sep (x:xs) = (uprint x) . sep . uprintSep sep xs


instance UnifiedPrint Hunk where
  uprint (Hunk hdr ls) = uprint hdr . nl . uprintSep nl ls
    
    

instance UnifiedPrint Header where
  uprint (UdHunkHeader r1 r2)
    = showString "@@" . space . printLeftRange r1 
        . space . printRightRange r2 . space . showString "@@"

printLeftRange, printRightRange :: Range -> ShowS
printLeftRange (a,b)  = dash . showInt a . comma . showInt b
printRightRange (a,b) = plus . showInt a . comma . showInt b

instance UnifiedPrint TimeStamp where
  uprint (InterpretedTime ct) = uprint ct
  uprint (StringTime s)       = str s

instance UnifiedPrint CalendarTime where
  uprint = isoTimeStamp

instance UnifiedPrint HunkLine where
  uprint (Added cs)     = str ('+':cs)            -- '+' prefix
  uprint (Removed cs)   = str ('-':cs)          -- '-' prefix 
  uprint (Common cs)    = str (' ':cs)       -- {space} prefix
  uprint Incomplete     = str "\\ No newline at end of file" 

--------------------------------------------------------------------------------
-- Printing CalendarTime
--------------------------------------------------------------------------------

isoTimeStamp :: CalendarTime -> ShowS
isoTimeStamp ts = isoDate ts . space . isoTime ts . space . isoTZ ts

-- yyyy-mm-dd
isoDate :: CalendarTime -> ShowS
isoDate (CalendarTime {ctYear=y, ctMonth=m, ctDay=d}) =
  int4 y . dash . int2Month m . dash . int2 d


-- hh:mm:ss.ppppppppp
isoTime :: CalendarTime -> ShowS
isoTime (CalendarTime {ctHour=h, ctMin=m, ctSec=s, ctPicosec=p}) =
  int2 h . colon . int2 m . colon . int2 s . dot . integer9 p

-- [-|+]zzzz
isoTZ :: CalendarTime -> ShowS
isoTZ (CalendarTime {ctTZ=z}) 
  | z < 0       = char '-' . int4 (abs z)
  | otherwise   = char '+' . int4 z

-- mm (adding 1 to make the sequence 01-12)
int2Month :: Month -> ShowS
int2Month = int2 . (+1) . fromEnum

tradTimeStamp :: CalendarTime -> ShowS
tradTimeStamp tm@(CalendarTime {ctYear=y, ctMonth=m, ctDay=d, ctWDay=wd}) = 
  dayStr3 wd . space . monthStr3 m . space . int2 d 
             . space . tradTime tm 
             . space . int4 y

tradTime :: CalendarTime -> ShowS
tradTime (CalendarTime {ctHour=h, ctMin=m, ctSec=s}) =
  int2 h . colon . int2 m . colon . int2 s

dayStr3 :: Day -> ShowS 
dayStr3 Sunday        = str "Sun"
dayStr3 Monday        = str "Mon"
dayStr3 Tuesday       = str "Tue"
dayStr3 Wednesday     = str "Wed"
dayStr3 Thursday      = str "Thu"
dayStr3 Friday        = str "Fri"
dayStr3 Saturday      = str "Sat"

monthStr3 :: Month -> ShowS
monthStr3 January     = str "Jan"
monthStr3 February    = str "Feb"
monthStr3 March       = str "Mar"
monthStr3 April       = str "Apr"
monthStr3 May         = str "May"
monthStr3 June        = str "Jun"
monthStr3 July        = str "Jul"
monthStr3 August      = str "Aug"
monthStr3 September   = str "Sep"
monthStr3 October     = str "Oct"
monthStr3 November    = str "Nov"
monthStr3 December    = str "Dec"
 

--------------------------------------------------------------------------------
-- Useful functions
--------------------------------------------------------------------------------

dash, colon, space, dot, comma, plus, tab, nl :: ShowS
dash    = ('-' :)
colon   = (':' :)
space   = (' ' :)
dot     = ('.' :)
comma   = (',' :)
plus    = ('+' :)
tab     = ('\t' :)
nl      = ('\n' :)

-- Shorthand versions 
str    = showString
char   = showChar 
int    = showInt

int2 :: Int -> ShowS
int2 n | n < 10 && n >=0     = str ('0': show n)
       | n < 100             = str (show n)
       | otherwise           = error $ "int2 value out of range " ++ show n 
       
       
int4 :: Int -> ShowS
int4 n | n >=0 && n <= 9999   = let s0 = show n; 
                                    x = 4 - (length s0);
                                in
                                str $ (replicate x '0') ++ s0
       | otherwise            = error $ "int4 value out of range " ++ show n

signed4 :: Int -> ShowS
signed4 n | n < 0     = ('-' :) . (int4 (abs n))
          | otherwise = ('+' :) . (int4 (abs n))


integer9 :: Integer -> ShowS
integer9 n | n >=0 && n < 1000000000  = let s0 = show n; 
                                             x = 9 - (length s0);
                                        in str $ (replicate x '0') ++ s0
           | otherwise                = error $ "int4 value out of range " ++ show n
