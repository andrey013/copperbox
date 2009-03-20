


## Make Haskell extension	
make-closure: ClosureSyn.hs Closure.hs Virtual.hs

# DATA only
ClosureSyn.hs: $(CLOSURE_DIR)/ClosureSyn.ag $(CLOSURE_DIR)/ClosureSynDEFS.ag
	uuagc -d $(CLOSURE_DIR)/ClosureSyn.ag -o $(CLOSURE_DIR)/ClosureSyn.hs

# SEM funs...
Closure.hs: $(CLOSURE_DIR)/Closure.ag
	uuagc -cfspw  $(CLOSURE_DIR)/Closure.ag	-o $(CLOSURE_DIR)/Closure.hs
	
Virtual.hs: $(CLOSURE_DIR)/Virtual.ag
	uuagc -cfspw  $(CLOSURE_DIR)/Virtual.ag -o $(CLOSURE_DIR)/Virtual.hs	
		
