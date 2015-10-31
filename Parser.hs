module Parser where

import Agpl_syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.Parsec
import Text.Parsec.Token

def :: LanguageDef st
def = emptyDef{ commentStart = "{-",
                commentEnd = "-}",
                commentLine = "--",
                nestedComments = True,
                identStart = letter,
                identLetter = alphaNum,
                opStart = oneOf "$=:|",
                opLetter = oneOf "$=:|",
                reservedNames = ["Gamestate", "Board", "Piece", "Turn", "Move",
                                 "isValid", "possMoves", "Hand", "outcome",
                                 "initialState", "Game"],
                reservedOpNames = ["(", ")", ":", ",", "."],
                caseSensitive = True}

TokenParser{ parens = m_parens,
             identifier = m_identifier,
             reservedOp = m_reservedOp,
             reserved = m_reserved,
             semiSep1 = m_semiSep1,
             whiteSpace = m_whiteSpace,
             stringLiteral = m_stringLiteral} = makeTokenParser def

gameParser :: Parser Game
gameParser = do {
               id <- gameIDParser;
               m_whiteSpace;
               m_reserved "Gamestate";
               m_reservedOp ":";
               gamestate <- many1 parseGameState;
               
               return NIL;
             }

gameIDParser :: Parser GameID
gameIDParser = do {
                 m_whiteSpace;
               id <- many (noneOf " :");
               m_whiteSpace;
               m_reservedOp ":";
               m_whiteSpace;
               m_reserved "Game";
               return id;
               }

parseGameState :: Parser GameState 
parseGameState = undefined

testparser = do {
               m_whiteSpace;
--               res <- m_stringLiteral;
               res <- many (noneOf " ,\n\0");
               return res;}

test :: String -> IO ()
test inp = case parse testparser "" inp of
             { Left err -> print err;
               Right ans -> print ans
             }

parseGame :: String -> IO Game
parseGame input = case parse gameParser "" input of
                    { Left err -> do {print err; return NIL};
                      Right ans -> return ans;}
                    
