all:
	mkdir -p build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc -Wall -O2 -XBangPatterns build/lex.hs build/parser.hs utils.hs main.hs -odir build -hidir ifaces -o main

pall:
	mkdir -p build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc -Wall -O2 -XBangPatterns -prof -fprof-auto -rtsopts build/lex.hs build/parser.hs annotator.hs -odir build -hidir ifaces -o annotator

prun:
	./annotator +RTS -p

run:
	./main

zip:
	zip task3.zip lex.x parser.y utils.hs main.hs Makefile
