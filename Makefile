SHELL=/bin/bash
BNFC?=/home/karolina/.local/bin/bnfc
all:
	-mkdir build
	cd src && \
	$(BNFC) Instant.cf && \
	# $(BNFC) --functor Instant.cf && \
	happy -gca ParInstant.y && \
	alex -g LexInstant.x && \
	ghc --make Main.hs -odir ../build -hidir ../build -o ../compiler

clean:
	-rm -rf build
	-rm -f src/{DocInstant,LexInstant,ParInstant,SkelInstant,PrintInstant,AbsInstant,ErrM,TestInstant}.*
	# -rm -f interpreter
