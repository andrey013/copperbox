
default: all

# directories
TOP_PREFIX=.


all: Syntax.hs Type.hs Infer.hs Pretty.hs

Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d Syntax.ag -o Syntax.hs

Type.hs: Type.ag TypeDEFS.ag
	uuagc -d Type.ag -o Type.hs

Infer.hs: Infer.ag SyntaxDEFS.ag TypeDEFS.ag 
	uuagc -cfspw  Infer.ag
	
Pretty.hs: Pretty.ag SyntaxDEFS.ag TypeDEFS.ag 
	uuagc -cfspw  Pretty.ag
	
clean:
	rm Syntax.hs; rm Type.hs; rm Pretty.hs

