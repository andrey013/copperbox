## This make file will be copied into the Gen directory
## It is located outside it so that the Gen directory can be 'rm *'-ed easily

GHC_OPTS=../template/ghc_opts.txt


all: base compiler ocaml attg haskell

## Make Base
base: AsdlConcreteSyn.hs AsdlCoreAbsSyn.hs OutputAsdl.hs \
      OutputHtml.hs


# this neeeds tidying up once we can pass file names to microasdl
AsdlConcreteSyn.hs: AsdlCoreAbsSynDEFS.ag
	mv AsdlCSAbsSynDEFS.ag AsdlConcreteSynDEFS.ag
	cp ../Base/uuag_src/AsdlConcreteSyn.ag . 
	uuagc -d AsdlConcreteSyn.ag -o AsdlConcreteSyn.hs

AsdlCoreAbsSyn.hs: AsdlCoreAbsSynDEFS.ag
	cp ../Base/uuag_src/AsdlCoreAbsSyn.ag . 
	uuagc -d AsdlCoreAbsSyn.ag -o AsdlCoreAbsSyn.hs
	
		
OutputAsdl.hs: AsdlCoreAbsSynDEFS.ag
	cp ../Base/uuag_src/OutputAsdl.ag .
	uuagc -cfsp --wrapper OutputAsdl.ag	

OutputHtml.hs: AsdlCoreAbsSynDEFS.ag
	cp ../Base/uuag_src/OutputHtml.ag .
	uuagc -cfsp --wrapper OutputHtml.ag	
	
## Make Compiler 
compiler: Asdl2Core.hs

Asdl2Core.hs : UuagAbsSynDEFS.ag
	cp ../Compiler/uuag_src/Asdl2Core.ag .
	uuagc -cfsp --wrapper Asdl2Core.ag  -o Asdl2Core.TEMP.hs
	cat $(GHC_OPTS) Asdl2Core.TEMP.hs > Asdl2Core.hs 
	rm Asdl2Core.TEMP.hs


## Make OCaml extension
ocaml: OCamlAbsSyn.hs AsdlCore2OCaml.hs OCamlGenAsdlPkl.hs \
		 OutputOCamlTypes.hs OutputOCamlExpr.hs
	

OCamlAbsSyn.hs: OCamlAbsSynDEFS.ag
	cp ../Ext/Caml/uuag_src/OCamlAbsSyn.ag . 
	uuagc -d OCamlAbsSyn.ag -o OCamlAbsSyn.hs



AsdlCore2OCaml.hs: UuagAbsSynDEFS.ag
	cp ../Ext/Caml/uuag_src/AsdlCore2OCaml.ag .
	uuagc -cfsp --wrapper AsdlCore2OCaml.ag  -o AsdlCore2OCaml.hs

OCamlGenAsdlPkl.hs: UuagAbsSynDEFS.ag
	cp ../Ext/Caml/uuag_src/OCamlGenAsdlPkl.ag .
	uuagc -cfsp --wrapper OCamlGenAsdlPkl.ag  -o OCamlGenAsdlPkl.hs
	
	
OutputOCamlTypes.hs: OCamlAbsSynDEFS.ag
	cp ../Ext/Caml/uuag_src/OutputOCamlTypes.ag .
	uuagc -cfsp --wrapper OutputOCamlTypes.ag
			
OutputOCamlExpr.hs: OCamlAbsSynDEFS.ag
	cp ../Ext/Caml/uuag_src/OutputOCamlExpr.ag .
	uuagc -cfsp --wrapper OutputOCamlExpr.ag
	
		
## Make UUAG extension	
attg: UuagAbsSyn.hs OutputUuagTypes.hs AsdlCore2Uuag.hs

UuagAbsSyn.hs: UuagAbsSynDEFS.ag
	cp ../Ext/UUAG/uuag_src/UuagAbsSyn.ag . 
	uuagc -d UuagAbsSyn.ag -o UuagAbsSyn.hs

OutputUuagTypes.hs: UuagAbsSynDEFS.ag
	cp ../Ext/UUAG/uuag_src/OutputUuagTypes.ag .
	uuagc -cfsp --wrapper OutputUuagTypes.ag	 	

AsdlCore2Uuag.hs: UuagAbsSynDEFS.ag
	cp ../Ext/UUAG/uuag_src/AsdlCore2Uuag.ag .
	uuagc -cfsp --wrapper AsdlCore2Uuag.ag  -o AsdlCore2Uuag.TEMP.hs
	cat $(GHC_OPTS) AsdlCore2Uuag.TEMP.hs > AsdlCore2Uuag.hs 
	rm AsdlCore2Uuag.TEMP.hs
	
## Make Haskell extension	
haskell: HsGenPickler.hs

HsGenPickler.hs: UuagAbsSynDEFS.ag
	cp ../Ext/Haskell/uuag_src/HsGenPickler.ag .
	uuagc -cfsp --wrapper HsGenPickler.ag  -o HsGenPickler.hs




