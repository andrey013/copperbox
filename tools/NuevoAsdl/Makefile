
default: explanation

include config.mk
include Base/base.mk
include Compiler/compiler.mk
include Ext/OCaml/ext-ocaml.mk
include Ext/Haskell/ext-haskell.mk
include Ext/UUAG/ext-uuag.mk
include LambdaCore/lambdacore.mk


explanation :
	@echo "make nuevoasdl"
	@echo "make regenerate   : regenerate all, needs nuevoasdl and uuag plus ghc 6.6"
	@echo "make gen-clean    : deletes _all_ files in the Gen directory"
	@echo "make -i clean     : remove all the .o and .hi files run with -i"

nuevoasdl : Main.hs ./Gen/Makefile ./Compiler/Compile.hs
	ghc --make Main.hs -i./External
	mv main.exe nuevoasdl.exe
	
regenerate : make-base make-compiler make-lambda-core make-ext-ocaml \
  make-ext-haskell make-ext-uuag
	
gen_makedirs:
	mkdir $(TOP_PREFIX)/Gen/Base
	mkdir $(TOP_PREFIX)/Gen/Compiler	
	mkdir $(TOP_PREFIX)/Gen/Ext
	mkdir $(TOP_PREFIX)/Gen/Ext/Haskell
	mkdir $(TOP_PREFIX)/Gen/Ext/OCaml
	mkdir $(TOP_PREFIX)/Gen/Ext/UUAG
	mkdir $(TOP_PREFIX)/Gen/LambdaCore

	
		
# Makefile note	- rm needs to be on the same line as cd (seperated by a semi ; )
gen-clean :
	cd Gen/ ; rm -rf *

.PHONY : clean
clean :
	rm Main.hi ; rm Main.o
	cd Base/ ; rm *.o ; rm *.hi 
	cd Compiler/ ; rm *.o ; rm *.hi 
	cd Compiler/Views ; rm *.o ; rm *.hi 
	cd Ext/Caml/ ; rm *.o ; rm *.hi 	
	cd Ext/UUAG/ ; rm *.o ; rm *.hi
	cd Ext/Haskell/ ; rm *.o ; rm *.hi 
	cd External/ ; rm *.o ; rm *.hi 
	cd Gen/ ; rm *.o ; rm *.hi 
	cd Util/ ; rm *.o ; rm *.hi

	