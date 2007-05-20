
module Text.Diff.Unified.Parser 
  ( parseUDiffFile
  )where

import Text.Diff.Unified.Datatypes


import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language





parseUDiffFile :: FilePath -> IO UdFile 
parseUDiffFile fname = 
  do{ ans <- parseFromFile udFile fname
    ; case ans of
        Left err -> error $ "Parse error: " ++ show err
        Right spec -> return spec
    }
    
udFile :: Parser UdFile
udFile =
  do{ hdr <- udFileHeader
    ; xs <- many udHunk
    ; return (UdFile hdr xs)
    }

udFileHeader :: Parser UdFileHeader
udFileHeader = 
  do{ reserved "---"
    ; ln <- srcFileName
    ; lt <- timeStamp
    ; reserved "+++"
    ; rn <- srcFileName
    ; rt <- timeStamp  
    ; return $ UdFileHeader { left_name=ln
                            , left_time=lt
                            , right_name=rn 
                            , right_time=rt
                            }
    }

udHunk :: Parser UdHunk 
udHunk  = 
  do{ hdr <- udHunkHeader
    ; ls <- many1 udLine
    ; return (UdHunk hdr ls)
    }

udHunkHeader :: Parser UdHunkHeader
udHunkHeader = 
  do{ reserved "@@"
    ; r1 <- rangeP (char '-')
    ; r2 <- rangeP (char '+')
    ; reserved "@@"
    ; return (UdHunkHeader r1 r2)
    }

rangeP :: Parser a -> Parser (Int,Int)
rangeP p =
  do{ p
    ; i1 <- integer
    ; comma
    ; i2 <- integer
    ; return (fromInteger i1, fromInteger i2)
    }
    

udLine :: Parser UdLine
udLine = added <|> removed <|> common <|> incomplete

added =
  do{ char '+'
    ; cs <- manyTill anyChar newline
    ; return (Added cs)
    }

removed =
  do{ char '-'
    ; cs <- manyTill anyChar newline
    ; return (Removed cs)
    }
    
common =
  do{ char ' '
    ; cs <- manyTill anyChar newline
    ; return (Common cs)
    }    
    
incomplete =
  do{ string "\\ No newline at end of file"
    ; newline
    ; return Incomplete
    }

timeStamp :: Parser TimeStamp
timeStamp =
  do{ (yr,mon,dy) <- dateTriple
    ; char ' '
    ; (hr,mn,sc,msc) <- timeQuad
    -- integer for msc takes the trailing space
    -- ; char ' '
    ; tz <- timeZone
    ; newline
    ; return $ TimeStamp
                  { udYear=yr 
                  , udMonth=mon
                  , udDay=dy  
                  , udHour=hr
                  , udMin=mn
                  , udSec=sc
                  , udSecFrac=msc
                  , udTimeZone=tz
                  } 
    }

dateTriple = 
  do{ year <- int4
    ; char '-'
    ; month <- int2
    ; char '-'
    ; day <- int2
    ; return (year,month,day)
    }

timeQuad =
  do{ hour <- int2
    ; char ':'
    ; minute <- int2
    ; char ':'
    ; second <- int2
    ; char '.'
    ; frac <- integer
    ; return (hour,minute,second,frac)
    }
    <?> "timeQuad"

-- note read won't read a '+' prefixed int
timeZone :: Parser Int  
timeZone = 
  do{ a <- char '-' <|> (char '+' >> return '0')
    ; as <- count 4 digit
    ; return (read (a:as))
    }
  
-- handily filenames are terminated by a tab
-- so we can easily handle spaces in them
srcFileName :: Parser SrcFileName
srcFileName = manyTill anyChar  (try (char '\t'))

    
--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------
udlexer            = P.makeTokenParser udDef

udDef = emptyDef 
          { reservedNames = ["@@", "---", "+++"]
          }

comma           = P.comma udlexer
reserved        = P.reserved udlexer    
integer         = P.integer udlexer

    

int2 :: Parser Int
int2 = intn 2 <?> "int2"
int4 = intn 4 <?> "int4"


intn :: Int -> Parser Int
intn n = do{ xs <- count n digit
           ; return (read xs)
           }           