
CABAL:=cabal

.PHONY: clean
clean:
	cabal clean

T1.eventlog:
	$(CABAL) --project-file=./cabal.project run T1 -- +RTS -l-au -p

cabal-clean.eventlog:
	$(CABAL) --project-file=cabal.project.spec-aggr -j run cabal-install:exe:cabal -- clean +RTS -l-au -p -olcabal-clean.eventlog

cabal-build-all.eventlog:
	$(CABAL) --project-file=./cabal.project -j run cabal-install:exe:cabal -- build specialist-test -j +RTS -l-au -p -olcabal-build-all.eventlog
