

module Sound.Bala.Format.LilyPond.LilyPondOutput where


import Text.PrettyPrint.Leijen hiding (dot)
import Data.Char

data Var = Var String Doc


var name doc | all isAlpha name = Var name doc
             | otherwise        = error $ "names must only contain letters"
def (Var name doc)  = text name <+> equals <+> doc
use (Var name _)  = char '\\' <> text name



qtext = dquotes . text

cmd str = char '\\' <> text str
elt str doc = text str <+> equals <+> doc 



putLy doc = putDoc $ version <$> doc <$> empty

simult :: Doc -> Doc
simult doc = text "<<" <+> doc <+> text ">>"

poly :: [Doc] -> Doc
poly = encloseSep (text "<< ") (text " >>") (text " \\\\ ")

version :: Doc
version = cmd "version" <+> qtext "2.10.3"

time :: Int -> Int -> Doc
time i j = cmd "time" <+> int i <> char '/' <> int j

tempo :: Int -> Int -> Doc
tempo i j = cmd "tempo" <+> int i <> equals <> int j




header :: Doc -> Doc 
header doc = cmd "header" <+> braces doc

score doc = cmd "score" <+> braces doc

title = elt "title" . qtext


relative note doc = cmd "relative" <+> note <+> doc

(#) = (<+>)
(##) = (<$>)

bar :: String -> Doc
bar str = cmd "bar" <+> qtext str

repeat_volta i doc = cmd "repeat" <+> text "volta" <+> int i <+> braces doc

dot doc = doc <> char '.'

rest i = r <> int i
r = char 'r'
r4 = rest 4
r8 = rest 8
r16 = rest 16


note = text
c4 = note "c4"
d4 = note "d4"
e4 = note "e4"
f4 = note "f4"
g4 = note "g4"
a4 = note "a4"
b4 = note "b4"

c8 = note "c8"
d8 = note "d8"
e8 = note "e8"
f8 = note "f8"
g8 = note "g8"
a8 = note "a8"
b8 = note "b8"

f'8 = note "f'8"
b_8 = note "b,8"
f_8 = note "f,8"
g_8 = note "g,8"


c16 = note "c16"
d16 = note "d16"
e16 = note "e16"
f16 = note "f16"
g16 = note "g16"
a16 = note "a16"
b16 = note "b16"

c'16 = note "c'16"

tie = char '~'

c'' = note "c''"
c'  = note "c'"
c__ = note "c,,"
c_  = note "c,"


midi doc = cmd "midi" <+> braces doc
layout doc = cmd "layout" <+> braces doc

midiInstrument str  = cmd "set" <+> elt "Staff.midiInstrument" (qtext str)


