
module Text.Diff.Unified.Printer
  (Render (..)
  ) where

import Text.Diff.Unified.Datatypes

import Numeric



-- We have render whitespace exactly therefore so don't use a pretty printer



dash, colon, space, dot, comma, plus, tab, nl :: ShowS
dash    = ('-' :)
colon   = (':' :)
space   = (' ' :)
dot     = ('.' :)
comma   = (',' :)
plus    = ('+' :)
tab     = ('\t' :)
nl      = ('\n' :)



class Render a where render :: a -> String

instance Render UdFile where
  render x = printUdFile x [] 

printUdFile :: UdFile -> ShowS
printUdFile (UdFile fhdr hs)
  = printUdFileHeader fhdr . nl . printUdHunks hs

printUdFileHeader :: UdFileHeader -> ShowS
printUdFileHeader (UdFileHeader name1 tstmp1 name2 tstmp2)
    = showString ("--- " ++ name1) . tab . printTimeStamp tstmp1
        . nl . showString ("+++ " ++ name2) . tab . printTimeStamp tstmp2 
    
printUdHunks :: [UdHunk] -> ShowS    
printUdHunks [] = showString ""
printUdHunks (x:xs) = (printUdHunk x) . nl . printUdHunks xs


printUdHunk :: UdHunk -> ShowS
printUdHunk (UdHunk hdr ls)
    = printUdHunkHeader hdr .  nl . printUdLines ls
    
    

printUdHunkHeader :: UdHunkHeader -> ShowS
printUdHunkHeader (UdHunkHeader r1 r2)
    = showString "@@ " . printLeftRange r1 
        . space . printRightRange r2 . showString " @@"

printLeftRange, printRightRange :: Range -> ShowS
printLeftRange (a,b)  = dash . showInt a . comma . showInt b
printRightRange (a,b) = plus . showInt a . comma . showInt b


printTimeStamp :: TimeStamp -> ShowS
printTimeStamp (TimeStamp { udYear=yr, udMonth=mon, udDay=dy
                          , udHour=hr, udMin=mins, udSec=secs
                          , udSecFrac=frac, udTimeZone=tz })
  = (int4 yr) . dash . (int2 mon) . dash . (int2 dy)
      . space . (int2 hr) . colon . (int2 mins) . colon . (int2 secs)
      . dot . (showString (show frac)) . space . (signed4 tz)


printUdLines :: [UdLine] -> ShowS
printUdLines [] = showString ""
printUdLines (x:xs) = (printUdLine x) . nl . printUdLines xs


printUdLine :: UdLine -> ShowS
printUdLine (Added cs)     = showString ('+':cs)            -- '+' prefix
printUdLine (Removed cs)   = showString ('-':cs)          -- '-' prefix 
printUdLine (Common cs)    = showString (' ':cs)       -- {space} prefix
printUdLine Incomplete     = showString "\\ No newline at end of file" 

int2 :: Int -> ShowS
int2 n | n < 10 && n >=0     = showString ('0': show n)
       | n < 100             = showString (show n)
       | otherwise           = error $ "int2 value out of range " ++ show n 
       
       
int4 :: Int -> ShowS
int4 n | n >=0 && n <= 9999   = let s0 = show n; 
                                    x = 4 - (length s0);
                                in
                                showString $ (replicate x '0') ++ s0
       | otherwise            = error $ "int4 value out of range " ++ show n

signed4 :: Int -> ShowS
signed4 n | n < 0     = ('-' :) . (int4 (abs n))
          | otherwise = ('+' :) . (int4 (abs n))


