
default: all

# directories
TOP_PREFIX=.
CLOSURE_DIR=$(TOP_PREFIX)/Closure
KNORMAL_DIR=$(TOP_PREFIX)/KNormal
SPARC_DIR=$(TOP_PREFIX)/Sparc

include Closure/closure.mk
include KNormal/knormal.mk
include Sparc/sparc.mk

all: Syntax.hs Type.hs   \
  Pretty.hs Typing.hs ToKNormal.hs \
  make-closure make-knormal make-sparc

Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d Syntax.ag -o Syntax.hs

Type.hs: Type.ag TypeDEFS.ag
	uuagc -d Type.ag -o Type.hs

Pretty.hs: Pretty.ag Syntax.hs Type.hs
	uuagc -cfspw  Pretty.ag
	
Typing.hs: Typing.ag Syntax.hs Type.hs
	uuagc -cfspw  Typing.ag

ToKNormal.hs: ToKNormal.ag Syntax.hs $(KNORMAL_DIR)/KNormalSyn.hs
	uuagc -cfspw  ToKNormal.ag -o ToKNormal.hs
	
clean:
	rm Syntax.hs; rm Type.hs; rm Typing.hs

