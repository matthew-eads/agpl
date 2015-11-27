#CC = ghc
#FLAGS = --make -XTemplateHaskell 

#all: Parser.hs Agpl_syntax.hs
#	${CC} ${FLAGS} -o parser Main.hs Parser.hs Agpl_syntax.hs
all: Parser.hs Agpl_syntax.hs Quote.hs
	stack build
clean: 
	stack clean; rm -f *~; rm -f *.hi; rm -f *.o;
 
