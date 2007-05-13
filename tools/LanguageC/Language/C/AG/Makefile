

Pretty.hs: Pretty.ag SyntaxExtra.ag SyntaxExtraDEFS.ag
	uuagc -d SyntaxExtra.ag -o SyntaxExtra.hs
	uuagc -cfsp --wrapper Pretty.ag	-o Pretty.hs


clean:
	rm *.hi ; rm *.o ; rm SyntaxExtra.hs ; rm Pretty.hs
	