all:
	mkdir -p build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc build/lex.hs build/parser.hs utils.hs -odir build -hidir ifaces -o simplifier

run:
	./simplifier

zip:
	zip task2.zip lex.x parser.y utils.hs Makefile
