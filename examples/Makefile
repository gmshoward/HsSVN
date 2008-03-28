GHCFLAGS = -fglasgow-exts

build:
	ghc $(GHCFLAGS) --make HelloWorld -threaded

run: build
	./HelloWorld

clean:
	rm -f HelloWorld *.hi *.o

.PHONY: build run clean
