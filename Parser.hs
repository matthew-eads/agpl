module Parser where

import Agpl_syntax
import Text.ParserCombinators.Parsec hiding (try)
import Text.ParserCombinators.Parsec.Language
import Text.Parsec (try)
import Text.Parsec.Token
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.Meta.Syntax.Translate
import Data.Functor.Identity
import Data.Matrix (Matrix, nrows, ncols, toList)
import Debug.Trace

iToC :: Int -> Int -> Int-> (Int, Int)
iToC rows cols i = let c = if (i `mod` cols) == 0 then cols else (i `mod` cols) 
                       r = ceiling ((realToFrac i) / (realToFrac rows))
                   in (r, c)

lfoldi :: (Int -> (Int, Int)) -> Int -> (((Int, Int), a, b) -> b) -> b -> [a] -> b
lfoldi toC i f b [] = b
lfoldi toC i f b (x:xs) = f ((toC i), x, (lfoldi toC (i-1) f b xs)) 

mfold :: (((Int, Int), a, b) -> b) -> b -> Matrix a -> b
mfold f b m = let nr = (nrows m)
                  nc = (ncols m)
                  l  = (toList m)
              in lfoldi (iToC nr nc) (length l) f b l

m_reservedNames = ["Gamestate", "Board", "Piece", "Turn", "Move",
                   "isValid", "possMoves", "Hand", "outcome",
                   "initialState", "Game", "Matrix"]

def :: LanguageDef st
def = emptyDef{ commentStart = "{-",
                commentEnd = "-}",
                commentLine = "--",
                nestedComments = True,
                identStart = letter,
                identLetter = alphaNum,
                opStart = oneOf "$=:|",
                opLetter = oneOf "$=:|",
                reservedNames = m_reservedNames,
                reservedOpNames = ["(", ")", ":", ",", ".", "{", "}"],
                caseSensitive = True}

TokenParser{ parens = m_parens,
             braces = m_braces,
             identifier = m_identifier,
             reservedOp = m_reservedOp,
             reserved = m_reserved,
             semiSep1 = m_semiSep1,
             whiteSpace = m_whiteSpace,
             stringLiteral = m_stringLiteral} = makeTokenParser def

gameParser :: Parser Game
gameParser = do {
               ws;
               id <- gameIDParser;
               ws;
               m_reserved "Gamestate";
               m_reservedOp ":";
               ws; m_reservedOp "{";
--               gamestate <- (manyTill parseGameState (m_reserved "}"));
               gamestate <- parseGameState; ws;
               m_reservedOp "}"; ws;
               player <- parsePlayer;
               move <- parseMove;
               isValid <- parseIsValid;
               possMoves <- parsePossMoves;
               outcome <- parseOutcome;
               initState <- parseInitState;
               return (Game (gamestate, move, isValid, possMoves, outcome, 
                            initState, player, []));
             }

parsePlayer :: Parser Player
parsePlayer = do {
              ws;
              m_reserved "Player";
              ws;
              m_reservedOp ":";
              ws;
              playerDec <- decParser "Player";
              return (Player playerDec);
}

parsePossMoves :: Parser PossMovesFun
parsePossMoves = do {
                   ws;
                   m_reserved "possMoves"; ws;
                   m_reservedOp ":"; ws;
                   possFun <- expParser;
                   return (PossMovesFun possFun);
                 }
                   
parseOutcome :: Parser OutcomeFun
parseOutcome = do {
                   ws;
                   m_reserved "outcome"; ws;
                   m_reservedOp ":"; ws;
                   outcomeFun <- expParser;
                   return (OutcomeFun outcomeFun);
                 }

parseIsValid :: Parser IsValidFun
parseIsValid = do {
                   ws;
                   m_reserved "isValid"; ws;
                   m_reservedOp ":"; ws;
                   isValidF <- expParser;
                   return (IsValidFun isValidF);
                 }

parseInitState :: Parser InitState
parseInitState = do {
                   ws;
                   m_reserved "initialState"; ws;
                   m_reservedOp ":"; ws;
                   initState <- expParser;
                   return (InitState initState);
                 }



parseMove :: Parser Move
parseMove = do {
              ws;
              m_reserved "Move";
              ws;
              m_reservedOp ":";
              ws;
              moveDec <- decParser "Move";
              return (Move moveDec);
            }

gameIDParser :: Parser GameID
gameIDParser = do {
                 ws;
                 id <- many (noneOf " :");
                 ws;
                 m_reservedOp ":";
                 ws;
                 m_reserved "Game";
                 return id;
               }

typeParser :: Parser Type
typeParser = do {
              ws;
              m_reservedOp "{";
              ty <- (many (noneOf "}"));
              m_reservedOp "}";
              case parseType ty of
                (Left err) -> do {trace "typeParser" (return undefined)}
                (Right typ) -> do {return typ}
            } 

expParser :: Parser Exp
expParser = do {
              ws;
              m_reservedOp "{";
              e <- (many (noneOf "}"));
              m_reservedOp "}";
--              trace ("parsing from: " ++ "(\\game -> " ++ e ++ ") :: Gamestate -> (Int, Int) -> Int") ws;
              case parseExp ("(\\game -> " ++ e ++ ") :: Gamestate -> (Int, Int) -> Int") of
                (Left err) -> do {trace ("expParser: " ++ err) (return undefined)}
                (Right exp) -> do {return exp}
            }
              
decParser :: String -> Parser Dec
decParser str = do {
              ws;
              m_reservedOp "{";
              dec <- (many (noneOf "}"));
              m_reservedOp "}";
              case parseDecs ("data " ++ str ++ " = " ++ dec) of
                (Right d) -> do {return (head d)}
                (Left err) -> case parseDecs ("data " ++ str ++ " = " ++ str ++ "(" ++ dec ++ ")") of
                                (Right d) -> do {return (head d)}
                                (Left err) -> do {trace "decParser" (return undefined)}
                }

matrixParser :: Parser Dec 
matrixParser = do {
                ws;
                m_reservedOp "{";
                m_reserved "Matrix";
                m_reservedOp "}";
                
                case  parseDecs ("data Board = Board (Matrix Piece)") of
                  (Right d) -> do {return (head d)}
                  (Left err) -> do {trace "matrixParser" (return undefined)} 
               }

boardParser :: Parser Dec
boardParser = do {
                ws;
                m_reserved "Board"; ws;
                m_reservedOp ":"; ws;
                t <- (try matrixParser <|> decParser "Board");
                return t;
}

pieceParser :: Parser Dec
pieceParser = do {
                ws;
                m_reserved "Piece"; ws;
                m_reservedOp ":"; ws;
                t <- decParser "Piece";
                return t;
}


handParser :: Parser Dec
handParser = do {
                ws;
                m_reserved "Hand"; ws;
                m_reservedOp ":"; ws;
                t <- decParser "Hand";
                return t;
}

turnParser :: Parser Dec
turnParser = do {
                ws;
                m_reserved "Turn"; ws;
                m_reservedOp ":"; ws;
                t <- decParser "Turn";
                return t;
}

customDParser :: Parser Dec
customDParser = do {
                ws;
                m_reserved "NOTSUPPORTED"; ws;
                m_reservedOp ":"; ws;
                t <- (try matrixParser <|> decParser "Board");
                return t;
}

nilParser :: Parser Dec
nilParser = case parseDecs "data NULL = NULL" of
              (Right d) -> do {return (head d)}
              (Left err) -> do {trace "nilParser" (return undefined)}


parseGameState :: Parser GameState 
parseGameState = do {
                   btypedec <- boardParser <|> nilParser;
                   nilT <- nilParser;
                   ptypedec <- pieceParser <|> nilParser;
                   htypedec <- handParser <|> nilParser;
                   ttypedec <- turnParser <|> nilParser;
                   ctypedec <- customDParser <|> nilParser;
                   return GameState 
                           {board=btypedec, piece=ptypedec,
                            hand=htypedec, turn=ttypedec, 
                            customData=ctypedec};}
                 
--                   ws;
--                   m_reserved "Piece
{-
                 (do {
                   m_reserved "Piece";
                   ws;
                   m_reservedOp ":";
                   ws;                   
                   typedec <- decParser "Piece";
                   ws;
                   return (Piece typedec) }) <|>
                 (do {
                   m_reserved "Turn";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser "Turn";
                   return (Turn typedec) }) <|>
                 (do {
                   m_reserved "Hand";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser "Hand";
                   ws;
                   return (Hand typedec) }) {-<|>
                 do { 
                   custID <- many (noneOf " :");
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser;
                   ws;
                   return (CustomData typedec) }-})
-}
{-parseGameState :: Parser GameState 
parseGameState = ws >> 
                 (do {
                   m_reserved "Board";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- boardParser;
                   return (Board typedec) } <|>
                 (do {
                   m_reserved "Piece";
                   ws;
                   m_reservedOp ":";
                   ws;                   
                   typedec <- decParser "Piece";
                   ws;
                   return (Piece typedec) }) <|>
                 (do {
                   m_reserved "Turn";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser "Turn";
                   return (Turn typedec) }) <|>
                 (do {
                   m_reserved "Hand";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser "Hand";
                   ws;
                   return (Hand typedec) }) {-<|>
                 do { 
                   custID <- many (noneOf " :");
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser;
                   ws;
                   return (CustomData typedec) }-})-}


                

testparser = do {
               ws;
--               res <- m_stringLiteral;
               res <- many (try (string "dog"));
--               res <- m_reserved;
               return res;}

test :: String -> IO ()
test inp = case parse testparser "" inp of
             { Left err -> print err;
               Right ans -> print ans
             }

parseGame :: String -> IO Game
parseGame input = case parse gameParser "" input of
                    { Left err -> do {print err; return NIL};
                      Right ans -> do {print "succ"; return ans}}

parseToS :: String -> String
parseToS input = case parse gameParser "" input of
                (Left err) -> (show NIL)
                (Right ans) -> (show ans)
                    
ws = m_whiteSpace;

