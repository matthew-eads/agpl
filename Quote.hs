module Quote (agpl) where
import Parser
import CodeGen
import Language.Haskell.TH
import Language.Haskell.TH.Quote

agpl :: QuasiQuoter
agpl = QuasiQuoter undefined undefined undefined aparse

aparse :: String -> Q [Dec]
aparse s = makeAGPLDecs (parseGame s)
