

LCORE_AG_SRC=$(LCORE_DIR)/uuag_src
LCORE_GEN_DIR=$(GEN_DIR)/LambdaCore


## Make LambdaCore intermediate representation
make-lambda-core: LambdaCoreAbsSyn.hs OutputLambdaCore.hs \
  TranslatePickle.hs


LambdaCoreAbsSyn.hs: $(DEFS_DIR)/lambda_core.asdl
	$(ASDL) --uuag -d $(LCORE_GEN_DIR) $(DEFS_DIR)/lambda_core.asdl
	uuagc -d $(LCORE_AG_SRC)/LambdaCoreAbsSyn.ag -o $(LCORE_GEN_DIR)/LambdaCoreAbsSyn.TEMP.hs	
	cat $(TEMPLATE_DIR)/ghc_opts.txt $(LCORE_GEN_DIR)/LambdaCoreAbsSyn.TEMP.hs > $(LCORE_GEN_DIR)/LambdaCoreAbsSyn.hs
	rm $(LCORE_GEN_DIR)/LambdaCoreAbsSyn.TEMP.hs
	

	
OutputLambdaCore.hs: $(DEFS_DIR)/lambda_core.asdl
	uuagc -cfsp --wrapper $(LCORE_AG_SRC)/OutputLambdaCore.ag  -o $(LCORE_GEN_DIR)/OutputLambdaCore.hs

TranslatePickle.hs: $(DEFS_DIR)/asdl_core.asdl $(DEFS_DIR)/lambda_core.asdl 
	uuagc -cfsp --wrapper $(LCORE_AG_SRC)/TranslatePickle.ag  -o $(LCORE_GEN_DIR)/TranslatePickle.hs
	