
all: Syntax.hs Type.hs KNormal.hs Alpha.hs Beta.hs Inline.hs Typing.hs

Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d Syntax.ag -o Syntax.hs

Type.hs: Type.ag TypeDEFS.ag
	uuagc -d Type.ag -o Type.hs

KNormal.hs: KNormal.ag KNormalDEFS.ag
	uuagc -d KNormal.ag -o KNormal.hs


Alpha.hs: Alpha.ag KNormal.hs Type.hs
	uuagc -cfspw Alpha.ag

Beta.hs: Beta.ag KNormal.hs Type.hs
	uuagc -cfspw  Beta.ag

Inline.hs: Inline.ag KNormal.hs Type.hs
	uuagc -cfspw  Inline.ag

Typing.hs: Typing.ag Syntax.hs Type.hs
	uuagc -cfspw  Typing.ag

clean:
	rm Syntax.hs; rm Type.hs; rm Typing.hs

