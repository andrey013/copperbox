
all: Syntax.hs Type.hs KNormalSyn.hs ClosureSyn.hs SparcAsmSyn.hs \
  Alpha.hs Beta.hs Closure.hs ConstFold.hs Emit.hs \
  Inline.hs KNormal.hs Simm13.hs SparcAsm.hs \
  ToClosure.hs ToKNormal.hs Typing.hs \
  Virtual.hs

Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d Syntax.ag -o Syntax.hs

Type.hs: Type.ag TypeDEFS.ag
	uuagc -d Type.ag -o Type.hs

KNormalSyn.hs: KNormalSyn.ag KNormalDEFS.ag
	uuagc -d KNormalSyn.ag -o KNormalSyn.hs

ClosureSyn.hs: ClosureSyn.ag ClosureSynDEFS.ag
	uuagc -d ClosureSyn.ag -o ClosureSyn.hs
	
SparcAsmSyn.hs: SparcAsmSyn.ag SparcAsmDEFS.ag
	uuagc -d SparcAsmSyn.ag -o SparcAsmSyn.hs
	

Alpha.hs: Alpha.ag KNormalSyn.hs Type.hs
	uuagc -cfspw Alpha.ag

Beta.hs: Beta.ag KNormalSyn.hs Type.hs
	uuagc -cfspw  Beta.ag

Closure.hs: Closure.ag ClosureSyn.hs Type.hs
	uuagc -cfspw  Closure.ag
	
ConstFold.hs: ConstFold.ag KNormalSyn.hs Type.hs
	uuagc -cfspw  ConstFold.ag
	
Emit.hs: Emit.ag SparcAsmSyn.hs
	uuagc -cfspw  Emit.ag
	
Inline.hs: Inline.ag KNormalSyn.hs
	uuagc -cfspw  Inline.ag

KNormal.hs: KNormal.ag KNormalSyn.hs
	uuagc -cfspw  KNormal.ag
	
	
SparcAsm.hs: SparcAsm.ag SparcAsmSyn.hs
	uuagc -cfspw  SparcAsm.ag
	
Simm13.hs: Simm13.ag SparcAsmSyn.hs
	uuagc -cfspw  Simm13.ag

ToClosure.hs: ToClosure.ag ClosureSyn.hs Type.hs
	uuagc -cfspw  ToClosure.ag

ToKNormal.hs: ToKNormal.ag KNormalSyn.hs Type.hs
	uuagc -cfspw  ToKNormal.ag
	
Typing.hs: Typing.ag Syntax.hs Type.hs
	uuagc -cfspw  Typing.ag

Virtual.hs: Virtual.ag SparcAsmSyn.hs ClosureSyn.hs
	uuagc -cfspw  Virtual.ag
	
clean:
	rm Syntax.hs; rm Type.hs; rm Typing.hs

