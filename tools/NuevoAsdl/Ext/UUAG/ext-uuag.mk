EXT_UUAG_AG_SRC=$(EXT_DIR)/UUAG/uuag_src
UUAG_GEN_DIR=$(GEN_DIR)/Ext/UUAG


## Make UUAG extension	
make-ext-uuag: UuagAbsSyn.hs AsdlCore2Uuag.hs OutputUuagTypes.hs 

UuagAbsSyn.hs: $(DEFS_DIR)/uuag.asdl
	$(ASDL) --uuag -d $(UUAG_GEN_DIR) $(DEFS_DIR)/uuag.asdl
	uuagc -d $(EXT_UUAG_AG_SRC)/UuagAbsSyn.ag -o $(UUAG_GEN_DIR)/UuagAbsSyn.hs

AsdlCore2Uuag.hs: $(DEFS_DIR)/uuag.asdl
	uuagc -cfsp --wrapper $(EXT_UUAG_AG_SRC)/AsdlCore2Uuag.ag  -o AsdlCore2Uuag.TEMP.hs
	cat $(GHC_OPTS) AsdlCore2Uuag.TEMP.hs > AsdlCore2Uuag.hs 
	rm AsdlCore2Uuag.TEMP.hs
	mv AsdlCore2Uuag.hs $(UUAG_GEN_DIR)/

OutputUuagTypes.hs: $(DEFS_DIR)/uuag.asdl
	uuagc -cfsp --wrapper $(EXT_UUAG_AG_SRC)/OutputUuagTypes.ag  -o $(UUAG_GEN_DIR)/OutputUuagTypes.hs



		
	