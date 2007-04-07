
COMP_AG_SRC=$(COMP_DIR)/uuag_src

COMP_DEPS=$(DEFS_DIR)/asdl_concrete_syn.asdl \
 $(DEFS_DIR)/asdl_core.asdl \

make-compiler: Asdl2Core.hs

Asdl2Core.hs : $(COMP_DEPS)
	uuagc -cfsp --wrapper $(COMP_AG_SRC)/Asdl2Core.ag  -o Asdl2Core.TEMP.hs
	cat $(GHC_OPTS) Asdl2Core.TEMP.hs > Asdl2Core.hs 
	rm Asdl2Core.TEMP.hs
	mv Asdl2Core.hs $(GEN_DIR)/Compiler/