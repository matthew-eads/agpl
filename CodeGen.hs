module CodeGen where

import Agpl_syntax
import Parser
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Debug.Trace
import Language.Haskell.Meta.Parse
import Data.Matrix as M hiding (trace) 
import Data.Vector hiding ((++), foldl, zipWith, drop, head, last)
nilD = (DataD [] (mkName "NULL") [] [NormalC (mkName "NULL") []] [])

makeAGPLDecs :: Game -> Q [Dec]
makeAGPLDecs (Game (id, gs, m, ivf, pmf, ocf, is, p, fs, cd, imports)) = 
    do {
      -- (trace ("\nmaking agpl decs from:\n" ++ (show (Game (id, gs, m, ivf, pmf, ocf, is, p, fs, cd, [])))) doNothing);
      gsdecs <- gamestateDec gs;
      -- (trace ("\ngsDecs:" ++ (show gsdecs)) doNothing);
      ttype <- turnTypeDec;
      -- (trace ("\nturntypeDecs:" ++ (show ttype)) doNothing);
      gsdec <- gsDec [boardT,turnT];
      -- (trace ("\ngameStateDec:" ++ (show gsdec)) doNothing);
      initStateDecs <- initStateDec is;
      -- (trace ("\ninitStateDecs:" ++ (show initStateDecs)) doNothing);
      move <- moveDec m;
      -- (trace ("\nmoveDecs:" ++ (show move)) doNothing);
      player <- playerDec p;
      -- (trace ("\nplayerDecs:" ++ (show player)) doNothing);
      isValid <- isValidDec ivf;
      -- (trace ("\nisvalidDecs:" ++ (show isValid)) doNothing);
      possMoves <- possmovesDec pmf;
      -- (trace ("\npossmovesDecs:" ++ (show possMoves)) doNothing);
      outcome <- outcomeDec ocf;
      -- (trace ("\noutcomeDecs:" ++ (show outcome)) doNothing);
      fromS <- fromStringDec fs;
      -- (trace ("\nfromstringDecs:" ++ (show fromS)) doNothing);
      inBounds <- inBoundsDec (board gs);
      -- (trace ("\ninboundsDecs:" ++ (show inBounds)) doNothing);
      isEmpty <- emptyDec (board gs);
      -- (trace ("\nisemptyDecs:" ++ (show isEmpty)) doNothing);
      place <- placeDec;
      -- (trace ("\nplaceDec:" ++ (show place)) doNothing);
      -- (trace ("\ninBounds: " ++ (show inBounds)) doNothing);
      return (imports ++ gsdecs ++ initStateDecs ++ ttype ++ move ++ player ++ isValid ++ gsdec ++ outcome ++ possMoves ++ fromS ++ cd ++ inBounds ++ isEmpty ++ place);
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
    let (bdec, sdec) = 
            case (board gs) of
              (Board d) -> (d, nilD)
              (Matrix (d, size)) -> (d, (sizeDec (show size))) 
              (Array (d, size)) -> (d, (sizeDec (show size)))
                 
        pdec = (piece gs)
        hdec = (hand gs)
        tdec = (turn gs)
    in do {
         return (foldl (\acc -> \x -> if x == nilD then acc else (x:acc)) 
                [] [bdec, sdec, pdec, hdec, tdec]) 
       }
      
sizeDec :: String -> Dec
sizeDec s = case parseExp s of
              (Right e) -> (ValD (VarP (mkName "size")) (NormalB e) [])
              (Left err) -> undefined

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
outcomeDec (CustOutcomeFun e) =
    let f = ValD (VarP (mkName "outcome")) (NormalB (e)) []
    in do {return [f]}

outcomeDec outcome = let winc = FunD (mkName "winc") [Clause [VarP (mkName "game")] (NormalB (wincon outcome)) []] 
                         tiec = FunD (mkName "tiec") [Clause [VarP (mkName "game")] (NormalB (tiecon outcome)) []] 
                         elsec = FunD (mkName "elsec") 
                                 [Clause [(VarP (mkName "game")), (VarP (mkName "move"))]
                                  (NormalB (elsecon outcome)) []] 
                     in case parseDecs ("outcome game move = if (winc game) then (Win (currentTurn game), 1)"
                                        ++ " else if (tiec game) then (Tie, 1)"
                                        ++ " else let g = elsec game move" 
                                        ++ " in if (winc g) then (Win (currentTurn game), 1)"
                                        ++ " else if (tiec g) then (Tie, 1) else (g, 1)") of
                          (Left err) -> do {return (trace err undefined)}
                          (Right ds) -> do {return ([elsec, winc, tiec] ++ ds)}

                       -- outcome game move = if ($(winc) (currentTurn game) (board game)) then (Win (currentTurn game), 1) else (Tie, 1) 



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
playerDec (Player (d, n)) = do {
                            otherPlayer <- otherPlayerDec d n;
                            return ([d] ++ otherPlayer);
                          }

conToName :: Con -> Name
conToName (NormalC name _) = name
conToName (RecC name _) = name
conToName (InfixC _ name _) = name
conToName _ = undefined

otherPlayerDec :: Dec -> Int -> Q [Dec]
otherPlayerDec player n = case player of 
                            (DataD [] _ [] players _) -> 
                                let playerNames = Prelude.map conToName players
                                    clauses = zipWith otherPlayerClause playerNames (drop 1 playerNames) 
                                              ++ [otherPlayerClause (last playerNames) (head playerNames)]
                                in do {return [FunD (mkName "otherPlayer") clauses]}

otherPlayerClause :: Name -> Name -> Clause
otherPlayerClause p1 p2 = Clause [ConP p1 []] (NormalB (ConE p2)) []

customDataDec :: CustomDataType -> Q [Dec]
customDataDec (CustomDataType d) = do {return [d]}

inBoundsDec :: Board -> Q [Dec]
inBoundsDec (Matrix (d, (x, y))) = 
    [d| inBounds (x1, y1) = ((x1 <= x) && (x1 > 0) && (y1 <= y) && (y1 > 0))|]
inBoundsDec (Array (d, x)) =
    [d| inBounds x = (x <= x && x > 0) |]
inBoundsDec _ = do {return []}

placeDec :: Q [Dec]
placeDec  = case parseDecs ("place game piece coord = let b = setElem piece coord (board game)\n"
                         ++ "                             t = otherPlayer (currentTurn game)\n"
                         ++ "                         in GameState{board=b, currentTurn=t}") of
                             (Right ds) -> do {return ds}
                             (Left err) -> do {(trace ("placeDec error: " ++ err) (return []))}

emptyDec :: Board -> Q [Dec]
emptyDec (Matrix _) = case parseDecs "isEmpty (i, j) game = (((board game) M.! (i,j)) == Nil)"
                         of (Right ds) -> do {return ds}
                            (Left err) -> do {return []}
emptyDec (Array _) = case parseDecs "isEmpty i game = (((board game) V.! i) == Nil)"
                     of (Right ds) -> do {return ds}
                        (Left err) -> do {return []}
emptyDec _ = do {return []}
