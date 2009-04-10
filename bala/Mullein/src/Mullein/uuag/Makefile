
default: all


all: ../AbcOutput.hs ../AbcSyntax.hs \
  ../LilyPondOutput.hs ../LilyPondSyntax.hs \
  ../Syntax.hs

../AbcOutput.hs: AbcOutput.ag ../AbcSyntax.hs
	uuagc -cfspw --newtypes AbcOutput.ag -o ../AbcOutput.hs
	
../AbcSyntax.hs: AbcSyntax.ag AbcSyntaxDEFS.ag
	uuagc -d --newtypes AbcSyntax.ag -o ../AbcSyntax.hs


../LilyPondOutput.hs: LilyPondOutput.ag ../LilyPondSyntax.hs
	uuagc -cfspw --newtypes LilyPondOutput.ag -o ../LilyPondOutput.hs

../LilyPondSyntax.hs: LilyPondSyntax.ag LilyPondSyntaxDEFS.ag
	uuagc -d --newtypes LilyPondSyntax.ag -o ../LilyPondSyntax.hs
		
../Syntax.hs: Syntax.ag SyntaxDEFS.ag
	uuagc -d --newtypes Syntax.ag -o ../Syntax.hs
	
clean:
	rm ../AbcOutput.hs ;
	rm ../AbcSyntax.hs ;
	rm ../LilyPondOutput.hs ;
	rm ../LilyPondSyntax.hs ;
	rm ../Syntax.hs ;

