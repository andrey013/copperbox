
INC = -I/usr/local/include


CIL_HOME=$(CODING)/ml/cil
CILLY=$(CIL_HOME)/obj/x86_WIN32/cilly.asm.exe




GCC=gcc

CC=$(CILLY)


	
	
cil-pickle:
	$(CILLY) --doatermpickle --keepunused test01.c
	$(CILLY) --doasdlpickle --keepunused test01.c

hsread:
	runhaskell -i../../haskell Test01.hs


compile:
	$(GCC) -o simple test01.c $(INC)
	
	
cil-help:
	$(CILLY) --help
	
	

			