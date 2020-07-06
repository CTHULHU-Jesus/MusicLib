all: Main

Main: Main.hs MusicLib.hs
	cabal build 2> errors.txt

.PHONY: run ghci
run:
	cabal run 2> errors.txt

ghci:
	cabal repl
