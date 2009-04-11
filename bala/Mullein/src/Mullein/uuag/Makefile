
default: all


all: ../Gen/AbcOutput.hs ../Gen/AbcSyntax.hs \
  ../Gen/AbcConvert.hs ../Gen/LilyPondConvert.hs \
  ../Gen/LilyPondOutput.hs ../Gen/LilyPondSyntax.hs \
  ../Gen/Syntax.hs

../Gen/AbcConvert.hs: AbcConvert.ag ../Gen/Syntax.hs ../Gen/AbcSyntax.hs
	uuagc -cfspw --newtypes AbcConvert.ag -o ../Gen/AbcConvert.hs

../Gen/AbcOutput.hs: AbcOutput.ag ../Gen/AbcSyntax.hs
	uuagc -cfspw --newtypes AbcOutput.ag -o ../Gen/AbcOutput.hs

../Gen/AbcSyntax.hs: AbcSyntax.ag AbcSyntaxDEFS.ag
	uuagc -d --newtypes AbcSyntax.ag -o ../Gen/AbcSyntax.hs

../Gen/LilyPondConvert.hs: LilyPondConvert.ag ../Gen/Syntax.hs ../Gen/LilyPondSyntax.hs
	uuagc -cfspw --newtypes LilyPondConvert.ag -o ../Gen/LilyPondConvert.hs

../Gen/LilyPondOutput.hs: LilyPondOutput.ag ../Gen/LilyPondSyntax.hs
	uuagc -cfspw --newtypes LilyPondOutput.ag -o ../Gen/LilyPondOutput.hs

../Gen/LilyPondSyntax.hs: LilyPondSyntax.ag LilyPondSyntaxDEFS.ag
	uuagc -d --newtypes LilyPondSyntax.ag -o ../Gen/LilyPondSyntax.hs

../Gen/Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d --newtypes Syntax.ag -o ../Gen/Syntax.hs

clean:
	rm ../Gen/*.hs ;

