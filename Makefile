CC = ghc

all: Parser.hs Agpl_syntax.hs
	${CC} Parser.hs Agpl_syntax.hs
