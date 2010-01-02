{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.JoinPrint
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Printing with /join-strings/.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint
  ( 
    Doc
  , empty
  , null
  , (<>)
  , (<+>)
  , (<$>)
  , text
  , char
  , int
  , integer

  , enclose
  , squotes
  , dquotes
  , parens
  , brackets
  , braces
  , angles

  , lparen
  , rparen
  , lbracket
  , rbracket
  , lbrace
  , rbrace
  , langle
  , rangle

  , replicateChar

  , padl
  , padr
  , column
  , column_truncr_padl
  , column_truncr_padr
  , column_truncl_padl
  , column_truncl_padr
  
  , center
  , center_truncr
  , center_truncl
  
  , render
  
  ) where


import Text.PrettyPrint.JoinPrint.JoinString ( JoinString, (++) )
import qualified Text.PrettyPrint.JoinPrint.JoinString as JS

import Prelude hiding ( (++), null, length )

newtype Doc = Doc { getDoc :: JoinString }
        
infixr 5 <$>
infixr 6 <>, <+>


empty :: Doc
empty = Doc $ JS.empty

null :: Doc -> Bool
null = JS.null . getDoc 

length :: Doc -> Int
length = JS.length . getDoc


(<>) :: Doc -> Doc -> Doc
Doc a <> Doc b = Doc $ a ++ b

(<+>) :: Doc -> Doc -> Doc
Doc a <+> Doc b = Doc (a ++ JS.cons1 ' ' b)

(<$>) :: Doc -> Doc -> Doc
Doc a <$> Doc b = Doc (a ++ JS.cons1 '\n' b)




text :: String -> Doc
text = Doc . (JS.text)

char :: Char -> Doc
char = Doc . (JS.text) . return

int :: Int -> Doc
int  = text . show

integer :: Integer -> Doc
integer = text . show


--------------------------------------------------------------------------------

enclose :: Doc -> Doc -> Doc -> Doc
enclose l r d = l <> d <> r

squotes :: Doc -> Doc
squotes = enclose (char '\'') (char '\'')

dquotes :: Doc -> Doc
dquotes = enclose (char '"') (char '"')


parens :: Doc -> Doc
parens = enclose lparen rparen

brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

braces :: Doc -> Doc
braces = enclose lbrace rbrace

angles :: Doc -> Doc
angles = enclose langle rangle



lparen :: Doc
lparen = char '('

rparen :: Doc
rparen = char ')'

lbracket :: Doc
lbracket = char '['

rbracket :: Doc
rbracket = char ']'

lbrace :: Doc
lbrace = char '{'

rbrace :: Doc
rbrace = char '}'

langle :: Doc
langle = char '<'

rangle :: Doc
rangle = char '>'


--------------------------------------------------------------------------------

replicateChar :: Int -> Char -> Doc
replicateChar i = Doc . (JS.text) . replicate i

padl :: Int -> Char -> Doc -> Doc
padl i c d = fn (length d) where
  fn dl | dl >= i   = d
        | otherwise = replicateChar (i-dl) c <> d 

padr :: Int -> Char -> Doc -> Doc
padr i c d = fn (length d) where
  fn dl | dl >= i   = d
        | otherwise = d <> replicateChar (i-dl) c

-- | column
column :: Int -> Char -> Doc -> Doc
column = column_truncr_padl


column_truncr_padl :: Int -> Char -> Doc -> Doc
column_truncr_padl i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeLeft i (getDoc d)
        | dl < i    = replicateChar (i-dl) c <> d
        | otherwise = d

column_truncr_padr :: Int -> Char -> Doc -> Doc
column_truncr_padr i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeLeft i (getDoc d)
        | dl < i    = d <> replicateChar (i-dl) c
        | otherwise = d



column_truncl_padl :: Int -> Char -> Doc -> Doc
column_truncl_padl i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeRight i (getDoc d)
        | dl < i    = replicateChar (i-dl) c <> d
        | otherwise = d

column_truncl_padr :: Int -> Char -> Doc -> Doc
column_truncl_padr i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeRight i (getDoc d)
        | dl < i    = d <> replicateChar (i-dl) c
        | otherwise = d

center :: Int -> Char -> Doc -> Doc
center = center_truncr


center_truncr :: Int -> Char -> Doc -> Doc
center_truncr i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeLeft i (getDoc d)
        | dl < i    = let (spl,spr) = centerSpacings (i-dl) c in
                      spl <> d <> spr
        | otherwise = d

center_truncl :: Int -> Char -> Doc -> Doc
center_truncl i c d = fn (length d) where
  fn dl | dl >  i   = Doc $ JS.takeRight i (getDoc d)
        | dl < i    = let (spl,spr) = centerSpacings (i-dl) c in
                      spl <> d <> spr
        | otherwise = d


centerSpacings :: Int -> Char -> (Doc,Doc)
centerSpacings i c = let (d,m) = i `divMod` 2 in
                     if m == 0 then dup $ replicateChar d c
                               else (replicateChar d c, replicateChar (d+1) c)
  where
    dup a = (a,a)


-- | Rendering is simple because there is no notion of fitting.
--
render :: Doc -> String
render = JS.toString . getDoc





{-
hexdump :: Int -> Int -> IOUArray Int Word8 -> IO ()
hexdump start end arr = do 
    elements <- unfoldrM eltPhi start
    return ()
  where
    offsets              = unfoldr locPhi start 
    num_width            = length $ showHex end ""
    locPhi p | p > end   = Nothing 
             | otherwise = Just (pad num_width '0' $ showHex p "", p+16)   

    eltPhi i | i > end   = return Nothing
             | otherwise = readArray arr i >>= \a ->
                           return (Just (repPair a,i+1))


hexdump start end elts = zipWith fn offsets data_lines 
  where
    fn l r      = l ++ (':':' ':r)
    
    offsets     = unfoldr locPhi start  -- infinite, but the zip will truncate
    num_width   = length $ showHex end ""
    locPhi p    = Just (pad num_width '0' $ showHex p "", p+16)   
    
    
    data_lines  = unfoldr dataPhi $ prefix ++ map repPair elts 
    prefix      = replicate (start `mod` 16) blank_pair 
    blank_pair  = ("  ",' ')
    repPair a   = (showHex a "", toPrint $ chr $ fromIntegral a)

    dataPhi []  = Nothing
    dataPhi xs  = let (ys,zs) = splitAt 16 xs in 
                  Just ([],zs) 


-- need a notion of columns...
                  
-- dataLine xs = let c = length xs in 
               


pad :: Int -> Char -> String -> String
pad i ch str = let len = i - length str in replicate len ch ++ str


revpad :: Int -> Char -> String -> String
revpad i ch str = let len = i - length str in str ++ replicate len ch

-}

{-

hex2 :: Integral a => a -> ShowS
hex2 a | a < 0      = showString "-ve"
       | a < 0x10   = showString "0x0" . showHex a
       | otherwise  = showString "0x"  . showHex a 


hex4 :: Integral a => a -> ShowS
hex4 a | a < 0      = showString "-ve"
       | a < 0x10   = showString "0x000" . showHex a
       | a < 0x100  = showString "0x00"  . showHex a 
       | a < 0x1000 = showString "0x0"   . showHex a 
       | otherwise  = showString "0x"    . showHex a 

hex8 :: Integral a => a -> ShowS
hex8 a | a < 0          = showString "-ve"
       | a < 0x10       = showString "0x0000000" . showHex a
       | a < 0x100      = showString "0x000000"  . showHex a 
       | a < 0x1000     = showString "0x00000"   . showHex a 
       | a < 0x10000    = showString "0x0000"    . showHex a 
       | a < 0x100000   = showString "0x000"     . showHex a 
       | a < 0x1000000  = showString "0x00"      . showHex a 
       | a < 0x10000000 = showString "0x0"       . showHex a 
       | otherwise      = showString "0x"        . showHex a 

-}