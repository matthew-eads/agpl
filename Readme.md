#AGPL (A Game Programming Language)

AGPL is a simple domain specific language embedded in Haskell, originally
designed for a language design course at Tufts University.

AGPL aims to provide a concise way to describe a variety of board and card 
games, while still providing enough expressiveness to allow for a great 
amount of flexibility.

The formal grammar of AGPL can be found in grammar.pdf, and a couple examples
are provided (ttt.agpl, and chess.agpl).

This project uses Stack, for instructions on installing Stack, please visit
https://github.com/commercialhaskell/stack#how-to-install 

To build using stack, run:
```
stack setup
stack build
```
To run the generated game, run the generated main file:
`./.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/main/main`

If you want to try chess, modify Main.hs so that the quasiquoted line 
`[agpl_f|ttt.agpl|]` is changed to `[agpl_f|chess.agpl|]`

You can create your own games and load them from a file like above,
or alternately quasiquote them directly using `[agpl|<agpl decs>|]`

----------------------------
###Releases:

1.1:	Added library support and basic standard library (Agpl_lib.hs)
	Added simpler syntax for common board types (Matrix and Array, Graph
	support to be added in later releases), allowing shorter code, as well
	as generation of helper functions such as `inBounds`.

1.0:     Fully working tic-tac-toe implementation.
	 Chess implementation partially functional, more work needed
	 to fully work, but language framework is sufficient.
	 More work should be done on improving ability to concisely
	 express games. AI not yet implemented. Card games not yet implemented.

