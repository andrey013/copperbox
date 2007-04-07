
BASE_AG_SRC=$(BASE_DIR)/uuag_src

make-base: AsdlConcreteSyn.hs AsdlCoreAbsSyn.hs OutputAsdl.hs OutputHtml.hs
      

AsdlConcreteSyn.hs: $(DEFS_DIR)/asdl_concrete_syn.asdl $(BASE_AG_SRC)/AsdlConcreteSyn.ag 
	$(ASDL) --uuag -d $(GEN_DIR)/Base $(DEFS_DIR)/asdl_concrete_syn.asdl
	uuagc -d $(BASE_AG_SRC)/AsdlConcreteSyn.ag -o $(GEN_DIR)/Base/AsdlConcreteSyn.hs

AsdlCoreAbsSyn.hs: $(DEFS_DIR)/asdl_core.asdl
	$(ASDL) --uuag -d $(GEN_DIR)/Base $(DEFS_DIR)/asdl_core.asdl
	uuagc -d $(BASE_AG_SRC)/AsdlCoreAbsSyn.ag -o $(GEN_DIR)/Base/AsdlCoreAbsSyn.hs


OutputAsdl.hs: $(DEFS_DIR)/asdl_core.asdl $(BASE_AG_SRC)/OutputAsdl.ag 
	uuagc -cfsp --wrapper $(BASE_AG_SRC)/OutputAsdl.ag	-o $(GEN_DIR)/Base/OutputAsdl.hs

OutputHtml.hs: $(DEFS_DIR)/asdl_core.asdl $(BASE_AG_SRC)/OutputHtml.ag
	uuagc -cfsp --wrapper $(BASE_AG_SRC)/OutputHtml.ag	-o $(GEN_DIR)/Base/OutputHtml.hs 


  