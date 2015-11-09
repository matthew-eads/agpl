module CodeGen where

import Agpl_syntax
import Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote

makeAGPLDecs :: Game -> Q [Dec]
makeAGPLDecs (Game (gs, m, ivf, pmf, ocf, is, p, cd)) =
    [d| foo x = x * x |] 
makeAGPLDecs _ = [d| foo y = y * y|]
