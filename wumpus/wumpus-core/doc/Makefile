
all: manual

manual: wumpus-core-manual.tex
	latex --output-directory=./out wumpus-core-manual.tex
	dvips -o ./out/wumpus-core-manual.ps ./out/wumpus-core-manual.dvi
	dvipdfm -o ./out/wumpus-core-manual.pdf ./out/wumpus-core-manual.dvi 


