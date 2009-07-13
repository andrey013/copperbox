
all: mullein

mullein: mullein.tex
	latex --output-directory=./out mullein.tex
	dvips -o ./out/mullein.ps ./out/mullein.dvi
	dvipdfm -o ./out/mullein.pdf ./out/mullein.dvi
	cp ./out/mullein.pdf ./..
