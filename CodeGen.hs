module CodeGen where

import Agpl_syntax
import Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Debug.Trace
nilD = (DataD [] (mkName "NULL") [] [NormalC (mkName "NULL") []] [])

makeAGPLDecs :: Game -> Q [Dec]
makeAGPLDecs (Game (id, gs, m, ivf, pmf, ocf, is, p, fs, cd)) = 
    do {
      (trace "\nmaking agpl decs\n" doNothing);
      gsdecs <- gamestateDec gs;
      (trace ("\ngsDecs:" ++ (show gsdecs)) doNothing);
      ttype <- turnTypeDec;
      (trace ("\nturntypeDecs:" ++ (show ttype)) doNothing);
      gsdec <- gsDec [boardT,turnT];
      (trace ("\ngameStateDec:" ++ (show gsdec)) doNothing);
      initStateDecs <- initStateDec is;
      (trace ("\ninitStateDecs:" ++ (show initStateDecs)) doNothing);
      move <- moveDec m;
      player <- playerDec p;
      isValid <- isValidDec ivf;
      possMoves <- possmovesDec pmf;
      outcome <- outcomeDec ocf;
      fromS <- fromStringDec fs;
      return (gsdecs ++ initStateDecs ++ ttype ++ move ++ player ++ isValid ++ gsdec ++ outcome ++ possMoves ++ fromS ++ cd);
    }
makeAGPLDecs x = (trace ("Error." ++ (show x)) undefined)
doNothing :: Q ()
doNothing = do {return ()}

gsDec :: [VarStrictType] -> Q [Dec]
gsDec types = do {return [DataD [] (mkName "GameState") [] 
                [(RecC (mkName "GameState")types),
                 (NormalC (mkName "Win") [(NotStrict,ConT (mkName "Player"))]),
                 (NormalC (mkName "Tie") [])] [(mkName "Show"), (mkName "Eq")]]}

boardT :: VarStrictType
boardT = ((mkName "board"), NotStrict, ConT (mkName "Board")) 

pieceT :: VarStrictType
pieceT = ((mkName "piece"), NotStrict, ConT (mkName "Piece"))

handT :: VarStrictType
handT = ((mkName "hand"), NotStrict, ConT (mkName "Hand"))

turnT :: VarStrictType
turnT = ((mkName "currentTurn"), NotStrict, ConT (mkName "Turn"))

turnTypeDec :: Q [Dec]
turnTypeDec = do {return [TySynD (mkName "Turn") [] 
                          (ConT (mkName "Player"))]}

gamestateDec :: GameState -> Q [Dec]
gamestateDec gs =
    let bdec = (board gs)
        pdec = (piece gs)
        hdec = (hand gs)
        tdec = (turn gs)
    in do {
         return (foldl (\acc -> \x -> if x == nilD then acc else (x:acc)) 
                [] [bdec, pdec, hdec, tdec]) 
       }
      
testD :: Q [Dec]
testD = [d| data A = A{b :: Int, c :: Char} |]
initStateDec :: InitState -> Q [Dec]
initStateDec is = 
    do {
      let board = (boardInit is) in
      let turn = ValD (VarP (mkName "turn")) (NormalB (turnInit is)) [] in
      let bDec = ValD (VarP (mkName "boardInitF")) (NormalB board) [] in
      return [bDec, turn];
    }

isValidDec :: IsValidFun -> Q [Dec]
isValidDec (IsValidFun e) =
    let f = ValD (VarP (mkName "isValid")) (NormalB (e)) []
    in do {return [f]}
      
outcomeDec :: OutcomeFun -> Q [Dec]
outcomeDec (OutcomeFun e) =
    let f = ValD (VarP (mkName "outcome")) (NormalB (e)) []
    in do {return [f]}

possmovesDec :: PossMovesFun -> Q [Dec]
possmovesDec (PossMovesFun e) =
    let f = ValD (VarP (mkName "possMoves")) (NormalB (e)) []
    in do {return [f]}
possmovesDec PMNil = do {return []}


fromStringDec :: FromString -> Q [Dec]
fromStringDec (FromString e) =
    let f = ValD (VarP (mkName "fromString")) (NormalB (e)) []
    in do {return [f]}

moveDec :: Move -> Q [Dec]
moveDec (Move d) = do {return [d]}

playerDec :: Player -> Q [Dec]
playerDec (Player d) = do {return [d]}

customDataDec :: CustomDataType -> Q [Dec]
customDataDec (CustomDataType d) = do {return [d]}
