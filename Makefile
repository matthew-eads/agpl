CC = ghc
FLAGS = --make -XTemplateHaskell 

all: Parser.hs Agpl_syntax.hs
	${CC} ${FLAGS} -o parser Main.hs Parser.hs Agpl_syntax.hs
