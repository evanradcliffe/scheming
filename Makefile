install:
	cabal update
	cabal install parsec

build:
	ghc -o main main.hs

run:
	.\main.exe

clean:
	del main.exe main.hi main.o
