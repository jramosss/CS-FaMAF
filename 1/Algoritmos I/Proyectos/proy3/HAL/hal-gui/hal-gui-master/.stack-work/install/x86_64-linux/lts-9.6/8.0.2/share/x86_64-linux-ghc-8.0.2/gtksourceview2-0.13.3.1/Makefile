
PROGS  = sourceview linenumber 
SOURCES = SourceView.hs LineNumber.hs

all : $(PROGS)

sourceview : SourceView.hs
	$(HC_RULE)

linenumber : LineNumber.hs
	$(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
