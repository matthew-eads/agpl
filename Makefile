CC = ghc
FLAGS = --make -XTemplateHaskell 

all: Parser.hs Agpl_syntax.hs
	${CC} ${FLAGS} Parser.hs Agpl_syntax.hs
