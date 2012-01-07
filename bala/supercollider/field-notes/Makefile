
all: field-notes

field-notes: field-notes.tex
	latex --output-directory=./out field-notes.tex
	dvips -o ./out/field-notes.ps ./out/field-notes.dvi
	dvipdfm -o ./out/field-notes.pdf ./out/field-notes.dvi 


