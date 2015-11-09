module Quote (agpl, agpl_f, aparse) where
import Parser
import CodeGen
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Agpl_syntax
import Debug.Trace
agpl :: QuasiQuoter
agpl = QuasiQuoter (trace "exp" undefined) (trace "pat" undefined) (trace "type" undefined) 
                   (trace "decs" aparse)

aparse :: String -> Q [Dec]
aparse s = (trace "hello" (makeAGPLDecs NIL))

agpl_f :: QuasiQuoter
agpl_f = quoteFile agpl
