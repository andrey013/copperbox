

all: CLexerAlt.hs CParserFrown.hs

demo: Demo.hs CLexerAlt.hs CParserFrown.hs CPretty.hs
	ghc --make Demo.hs
	
CLexerAlt.hs: CLexerAlt.x ./alex_templates/AlexWrapper-errstate
	alex --ghc --template=./alex_templates CLexerAlt.x
	

# NOTE
# Don't use --code=compact
# 'compact' doesn't generate type signatures and without them GHC spins 
# out of control
CParserFrown.hs: CParserFrown.g
	frown --ghc --lexer --optimize --signature=poly --code=stackless --verbose CParserFrown.g

clean:
	rm CParserFrown.hs* ; rm CLexerAlt.hs ; rm *.o ; rm *.exe ; rm *.hi