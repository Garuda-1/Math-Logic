all:
	mkdir -p build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc -Wall -O4 -XBangPatterns build/lex.hs build/parser.hs utils.hs axioms.hs proofs.hs cheats.hs main.hs -odir build -hidir ifaces -o main

pall:
	mkdir -p build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc -Wall -O2 -XBangPatterns -prof -fprof-auto -rtsopts build/lex.hs build/parser.hs utils.hs axioms.hs proofs.hs cheats.hs main.hs -odir build -hidir ifaces -o main

prun:
	./main +RTS -p

run:
	./main

zip:
	zip task4.zip lex.x parser.y utils.hs proofs.hs axioms.hs cheats.hs main.hs Makefile
