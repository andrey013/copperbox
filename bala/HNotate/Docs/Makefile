

all: parts top

top: HNotateGuide.lhs
	lhs2TeX -o out/HNotate.tex HNotateGuide.lhs
	latex -output-directory=out out/HNotate.tex
	dvips -o out/HNotate.ps out/HNotate.dvi
	dvipdfm -o out/HNotate.pdf out/HNotate.dvi

parts: LyCommands.lhs AbcFields.lhs
	lhs2TeX -o out/LyCommands.tex LyCommands.lhs
	lhs2TeX -o out/AbcFields.tex AbcFields.lhs


	
clean:
	rm out/HNotate.*
			