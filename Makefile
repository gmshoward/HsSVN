CABAL_FILE = HsSVN.cabal
GHC = ghc

build: dist/setup-config Setup
	./Setup build

run: build
	@echo ".:.:. Let's go .:.:."
	$(MAKE) -C examples run

dist/setup-config: $(CABAL_FILE) configure Setup
	env EXTRA_CPPFLAGS="-I/usr/pkg/include/subversion-1 -I/usr/pkg/include/apr-1" EXTRA_LDFLAGS="-L/usr/pkg/lib" ./Setup configure -O
#	env EXTRA_CPPFLAGS="-I/usr/pkg/include/subversion-1 -I/usr/pkg/include/apr-1" EXTRA_LDFLAGS="-L/usr/pkg/lib" ./Setup configure --disable-optimization

configure: aclocal.m4 configure.ac
	autoconf

aclocal.m4:
	aclocal
	touch aclocal.m4

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
