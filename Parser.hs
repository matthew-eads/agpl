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


m_reservedNames = ["Gamestate", "Board", "Piece", "Turn", "Move",
                   "isValid", "possMoves", "Hand", "outcome",
                   "initialState", "Game"]

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
               gamestate <- (manyTill parseGameState (m_reserved "}"));
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
              playerDec <- decParser;
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
              moveDec <- decParser;
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

decParser :: Parser Type
decParser = do {
              ws;
              m_reservedOp "{";
              ty <- (many (noneOf "}"));
              m_reservedOp "}";
              case parseType ty of
                (Left err) -> do {return undefined}
                (Right typ) -> do {return typ}
            } 

expParser :: Parser Exp
expParser = do {
              ws;
              m_reservedOp "{";
              e <- (many (noneOf "}"));
              m_reservedOp "}";
              case parseExp e of
                (Left err) -> do {return undefined}
                (Right exp) -> do {return exp}
            }
              


parseGameState :: Parser GameState 
parseGameState = ws >> 
                 (do {
                   m_reserved "Board";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser;
                   return (Board typedec) } <|>
                 (do {
                   m_reserved "Piece";
                   ws;
                   m_reservedOp ":";
                   ws;                   
                   typedec <- decParser;
                   ws;
                   return (Piece typedec) }) <|>
                 (do {
                   m_reserved "Turn";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser;
                   return (Turn typedec) }) <|>
                 (do {
                   m_reserved "Hand";
                   ws;
                   m_reservedOp ":";
                   ws;
                   typedec <- decParser;
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
                    
ws = m_whiteSpace;

