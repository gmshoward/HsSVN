GHCFLAGS = -fglasgow-exts

build:
	ghc $(GHCFLAGS) --make HelloWorld

run: build
	./HelloWorld

clean:
	rm -f HelloWorld *.hi *.o

.PHONY: build run clean
