EXT_HS_AG_SRC=$(EXT_DIR)/Haskell/uuag_src
HS_GEN_DIR=$(GEN_DIR)/Ext/Haskell


## Make Haskell extension	
make-ext-haskell: HaskellAbsSyn.hs OutputHaskell.hs HsGenPickler.hs HsGenDatatypes.hs

HaskellAbsSyn.hs: $(DEFS_DIR)/haskell.asdl
	$(ASDL) --uuag -d $(HS_GEN_DIR) $(DEFS_DIR)/haskell.asdl
	uuagc -d $(EXT_HS_AG_SRC)/HaskellAbsSyn.ag -o $(HS_GEN_DIR)/HaskellAbsSyn.hs
	

OutputHaskell.hs: $(DEFS_DIR)/haskell.asdl
	uuagc -cfsp --wrapper $(EXT_HS_AG_SRC)/OutputHaskell.ag  -o $(HS_GEN_DIR)/OutputHaskell.hs

	
		
HsGenPickler.hs: $(DEFS_DIR)/haskell.asdl
	uuagc -cfsp --wrapper $(EXT_HS_AG_SRC)/HsGenPickler.ag  -o $(HS_GEN_DIR)/HsGenPickler.hs

HsGenDatatypes.hs: $(DEFS_DIR)/haskell.asdl
	uuagc -cfsp --wrapper $(EXT_HS_AG_SRC)/HsGenDatatypes.ag  -o $(HS_GEN_DIR)/HsGenDatatypes.hs
	
	