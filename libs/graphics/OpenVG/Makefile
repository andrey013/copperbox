# Makefile is for my use only, so I can invoke the correct 
# GHC/runhaskell on my Windows machine. 


RUNHASK = c:/ghc/HP/2009.2.0.2/bin/runhaskell.exe
GHC = c:/ghc/HP/2009.2.0.2/bin/ghc.exe
GHC_PKG = c:/ghc/HP/2009.2.0.2/bin/ghc-pkg.exe

haddock: 
	$(RUNHASK) Setup.hs configure
	$(RUNHASK) Setup.hs haddock --hyperlink-source

configure:
	$(RUNHASK) Setup.hs configure

clean: 
	$(RUNHASK) Setup.hs clean

build:
	$(RUNHASK) Setup.hs configure
	$(RUNHASK) Setup.hs build

sdist: 
	$(RUNHASK) Setup.hs configure
	$(RUNHASK) Setup.hs sdist

install:
	$(RUNHASK) Setup.hs install

unregister: 
	$(RUNHASK) Setup.hs unregister

pkglist:
	$(GHC_PKG) list 

# seems to hang msys...
test:
	$(RUNHASK) -i./demo -lopenvg32 demo/TestVgu.hs &


