
include config.mk


explanation :
	@echo "make nuevoasdl"
	@echo "make regenerate   : regenerate all, needs nuevoasdl and uuag plus ghc 6.6"
	@echo "make gen-clean    : deletes _all_ files in the Gen directory"
	@echo "make -i clean     : remove all the .o and .hi files run with -i"

nuevoasdl : Main.hs ./Gen/Makefile ./Compiler/Compile.hs
	ghc --make Main.hs -i./External
	mv main.exe nuevoasdl.exe
	
regenerate :
	$(ASDL) --uuag -d Gen/ Asdl_Defs/asdl_cs.asdl
	$(ASDL) --uuag -d Gen/ Asdl_Defs/asdl_core.asdl
	$(ASDL) --uuag -d Gen/ Asdl_Defs/uuag.asdl
	$(ASDL) --uuag -d Gen/ Asdl_Defs/ocaml.asdl
	$(ASDL) --uuag -d Gen/ Asdl_Defs/haskell.asdl
	cp Gen-Makefile Gen/Makefile
	cd Gen && \
	make
	
		
# Makefile note	- rm needs to be on the same line as cd (seperated by a semi ; )
gen-clean :
	cd Gen/ ; rm *

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

	