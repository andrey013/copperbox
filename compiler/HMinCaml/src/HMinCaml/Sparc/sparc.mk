


## Make Haskell extension	
make-sparc: SparcSyn.hs Emit.hs RegAlloc.hs SparcAsm.hs Simm13.hs

# DATA only
SparcSyn.hs: $(SPARC_DIR)/SparcSyn.ag $(SPARC_DIR)/SparcSynDEFS.ag
	uuagc -d $(SPARC_DIR)/SparcSyn.ag -o $(SPARC_DIR)/SparcSyn.hs

# SEM funs...
Emit.hs: $(SPARC_DIR)/Emit.ag
	uuagc -cfspw  $(SPARC_DIR)/Emit.ag	-o $(SPARC_DIR)/Emit.hs

RegAlloc.hs: $(SPARC_DIR)/RegAlloc.ag
	uuagc -cfspw  $(SPARC_DIR)/RegAlloc.ag -o $(SPARC_DIR)/RegAlloc.hs
			
SparcAsm.hs: $(SPARC_DIR)/SparcAsm.ag
	uuagc -cfspw  $(SPARC_DIR)/SparcAsm.ag -o $(SPARC_DIR)/SparcAsm.hs
	
Simm13.hs: $(SPARC_DIR)/Simm13.ag
	uuagc -cfspw  $(SPARC_DIR)/Simm13.ag -o $(SPARC_DIR)/Simm13.hs

