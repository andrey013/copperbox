

frown: Lexer.hs Parser.hs

Lexer.hs: Lexer.x ./alex_templates/AlexWrapper-clexer
	alex --ghc --template=./alex_templates Lexer.x
	

# NOTE
# Don't use --code=compact
# 'compact' doesn't generate type signatures and without them GHC spins 
# out of control
	
Parser.hs: Parser.g
	frown --ghc --expected --lexer --optimize --signature=poly --code=stackless --verbose Parser.g

clean:
	rm *.hi; rm *.o; rm Parser.hs* ; rm Lexer.hs
	