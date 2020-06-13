all:
	mkdir -p build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc build/lex.hs build/parser.hs annotator.hs -odir build -hidir ifaces -o annotator

run:
	./annotator

zip:
	zip task3.zip lex.x parser.y annotator.hs Makefile
