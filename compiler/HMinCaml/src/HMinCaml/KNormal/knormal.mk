


## Make Haskell extension	
make-knormal: KNormalSyn.hs Alpha.hs Assoc.hs Beta.hs ConstFold.hs \
  Elim.hs Inline.hs KNormal.hs KNormalToClosure.hs

# DATA only
KNormalSyn.hs: $(KNORMAL_DIR)/KNormalSyn.ag $(KNORMAL_DIR)/KNormalSynDEFS.ag
	uuagc -d $(KNORMAL_DIR)/KNormalSyn.ag -o $(KNORMAL_DIR)/KNormalSyn.hs

# SEM funs...
Alpha.hs: $(KNORMAL_DIR)/Alpha.ag
	uuagc -cfspw  $(KNORMAL_DIR)/Alpha.ag -o $(KNORMAL_DIR)/Alpha.hs

Assoc.hs: $(KNORMAL_DIR)/Assoc.ag
	uuagc -cfspw  $(KNORMAL_DIR)/Assoc.ag -o $(KNORMAL_DIR)/Assoc.hs
			
Beta.hs: $(KNORMAL_DIR)/Beta.ag
	uuagc -cfspw  $(KNORMAL_DIR)/Beta.ag -o $(KNORMAL_DIR)/Beta.hs
	
ConstFold.hs: $(KNORMAL_DIR)/ConstFold.ag
	uuagc -cfspw  $(KNORMAL_DIR)/ConstFold.ag -o $(KNORMAL_DIR)/ConstFold.hs

Elim.hs: $(KNORMAL_DIR)/Elim.ag
	uuagc -cfspw  $(KNORMAL_DIR)/Elim.ag -o $(KNORMAL_DIR)/Elim.hs
	
Inline.hs: $(KNORMAL_DIR)/Inline.ag
	uuagc -cfspw  $(KNORMAL_DIR)/Inline.ag -o $(KNORMAL_DIR)/Inline.hs

KNormal.hs: $(KNORMAL_DIR)/KNormal.ag
	uuagc -cfspw  $(KNORMAL_DIR)/KNormal.ag -o $(KNORMAL_DIR)/KNormal.hs
	
KNormalToClosure.hs: $(KNORMAL_DIR)/KNormalToClosure.ag
	uuagc -cfspw  $(KNORMAL_DIR)/KNormalToClosure.ag -o $(KNORMAL_DIR)/KNormalToClosure.hs

			