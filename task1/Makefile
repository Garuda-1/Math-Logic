all:
	mkdir build
	cd build
	alex ../lex.x
	happy ../parser.y
	ghc lex.hs parser.hs

run:
	build/parser
