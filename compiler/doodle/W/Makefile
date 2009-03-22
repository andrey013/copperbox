
default: all


all: Syntax.hs Infer.hs Pretty.hs

Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d Syntax.ag -o Syntax.hs

Infer.hs: Infer.ag Syntax.hs
	uuagc -cfspw  Infer.ag

Pretty.hs: Pretty.ag Syntax.hs
	uuagc -cfspw  Pretty.ag
	
clean:
	rm Syntax.hs; rm Infer.hs; Pretty.hs

