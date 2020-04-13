all:
	mkdir -p build
	cd build
	alex lex.x -o build/lex.hs
	happy parser.y -o build/parser.hs
	ghc build/lex.hs build/parser.hs -o build/parser

run:
	build/parser

zip:
	zip task1.zip lex.x parser.y Makefile
