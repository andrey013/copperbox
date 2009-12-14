# Makefile is for my use only, so I can invoke the correct 
# GHC/runhaskell on my Windows machine. 


RUNHASK = c:/ghc/HP/2009.2.0.2/bin/runhaskell.exe

haddock: 
	$(RUNHASK) Setup.hs configure
	$(RUNHASK) Setup.hs haddock

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

test:
	$(RUNHASK) -i./examples -lopenvg32 examples/TestVgu.hs