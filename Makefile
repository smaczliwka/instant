SHELL=/bin/bash
BNFC?=/home/karolina/.local/bin/bnfc
all:
	-mkdir build
	cd src && \
	$(BNFC) Instant.cf && \
	# $(BNFC) --functor Instant.cf && \
	happy -gca ParInstant.y && \
	alex -g LexInstant.x && \
	ghc --make JVMMain.hs -odir ../build -hidir ../build -o ../insc_jvm && \
	ghc --make LLVMMain.hs -odir ../build -hidir ../build -o ../insc_llvm

clean:
	-rm -rf build
	-rm -f src/{DocInstant,LexInstant,ParInstant,SkelInstant,PrintInstant,AbsInstant,ErrM,TestInstant}.*
	# -rm -f interpreter
