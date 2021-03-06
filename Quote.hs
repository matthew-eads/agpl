module Quote (agpl, agpl_f, aparse) where
import Parser
import CodeGen
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Agpl_syntax
import Debug.Trace
agpl :: QuasiQuoter
agpl = QuasiQuoter undefined undefined undefined aparse

aparse :: String -> Q [Dec]
aparse s = (makeAGPLDecs (parseGame s))

agpl_f :: QuasiQuoter
agpl_f = quoteFile agpl
