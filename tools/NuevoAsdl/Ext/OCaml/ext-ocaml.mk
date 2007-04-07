
EXT_OCAML_AG_SRC=$(EXT_DIR)/OCaml/uuag_src
OCAML_GEN_DIR=$(GEN_DIR)/Ext/OCaml



## Make OCaml extension
make-ext-ocaml: OCamlAbsSyn.hs AsdlCore2OCaml.hs OCamlGenAsdlPkl.hs \
		 OutputOCamlTypes.hs OutputOCamlExpr.hs
	

OCamlAbsSyn.hs: $(DEFS_DIR)/ocaml.asdl
	$(ASDL) --uuag -d $(OCAML_GEN_DIR) $(DEFS_DIR)/ocaml.asdl
	uuagc -d $(EXT_OCAML_AG_SRC)/OCamlAbsSyn.ag -o $(OCAML_GEN_DIR)/OCamlAbsSyn.hs	


AsdlCore2OCaml.hs: $(DEFS_DIR)/ocaml.asdl
	uuagc -cfsp --wrapper $(EXT_OCAML_AG_SRC)/AsdlCore2OCaml.ag  -o $(OCAML_GEN_DIR)/AsdlCore2OCaml.hs

OCamlGenAsdlPkl.hs: $(DEFS_DIR)/ocaml.asdl
	uuagc -cfsp --wrapper $(EXT_OCAML_AG_SRC)/OCamlGenAsdlPkl.ag  -o $(OCAML_GEN_DIR)/OCamlGenAsdlPkl.hs
	
	
OutputOCamlTypes.hs: $(DEFS_DIR)/ocaml.asdl
	uuagc -cfsp --wrapper $(EXT_OCAML_AG_SRC)/OutputOCamlTypes.ag -o $(OCAML_GEN_DIR)/OutputOCamlTypes.hs
			
OutputOCamlExpr.hs: $(DEFS_DIR)/ocaml.asdl
	uuagc -cfsp --wrapper $(EXT_OCAML_AG_SRC)/OutputOCamlExpr.ag -o $(OCAML_GEN_DIR)/OutputOCamlExpr.hs



