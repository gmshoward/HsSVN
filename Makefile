CABAL_FILE = HsSVN.cabal
GHC = ghc

build: dist/setup-config Setup
	./Setup build

run: build
	@echo ".:.:. Let's go .:.:."
	$(MAKE) -C examples run

dist/setup-config: $(CABAL_FILE) configure Setup
	./Setup configure -O \
		--configure-option="--with-subversion-prefix=/usr/pkg"

configure: configure.ac
	autoreconf -i

Setup: Setup.hs
	$(GHC) --make Setup

clean:
	rm -rf dist Setup Setup.o Setup.hi *.buildinfo
	find . -name '*~' -exec rm -f {} \;
	$(MAKE) -C examples clean

doc: dist/setup-config Setup
	./Setup haddock

install: build
	sudo ./Setup install

sdist: Setup
	./Setup sdist

.PHONY: build run clean install doc sdist
