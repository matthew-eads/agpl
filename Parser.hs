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
import System.IO.Unsafe

m_reservedNames = ["Gamestate", "Board", "Piece", "Turn", "Move",
                   "isValid", "possMoves", "Hand", "outcome",
                   "initialState", "Game", "Matrix", "all", "winCondition"]

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
                reservedOpNames = ["(", ")", ":", ",", ".", "{", "}", ">>", "<<"],
                caseSensitive = True}

TokenParser{ parens = m_parens,
             braces = m_braces,
             brackets = m_brackets,
             natural = m_natural,
             identifier = m_identifier,
             reservedOp = m_reservedOp,
             reserved = m_reserved,
             semiSep1 = m_semiSep1,
             whiteSpace = m_whiteSpace,
             stringLiteral = m_stringLiteral} = makeTokenParser def

reservedParser = foldl (\acc -> \s -> m_reserved s <|> acc)
                       (m_reserved "Game") m_reservedNames

emptyDecL :: Parser [Dec]
emptyDecL = do {return []} 

gameParser :: Parser Game
gameParser = do {
               ws;
               importsL <- many parseImports; ws; -- <|> emptyDecL; ws;
               -- (trace "got imports" ws);
               let imports = foldl (++) [] importsL in do {
               -- (trace ("parsed imports: " ++ (show imports)) ws);
               id <- gameIDParser; ws; 
               -- (trace ("id is: " ++ id) ws);
               
               m_reserved "Gamestate"; ws;
               m_reservedOp ":"; ws;
               m_reservedOp "{";
--               gamestate <- (manyTill parseGameState (m_reserved "}"));
               gamestate <- parseGameState; ws;
               -- (trace ("gs: " ++ (show gamestate)) ws);
               m_reservedOp "}"; ws;
               player <- parsePlayer; ws;
               -- (trace ("\n\nplayer: " ++ (show player)) ws);
               move <- parseMove; ws;
               -- (trace ("\n\nmove: " ++ (show move)) ws);
               isValid <- parseIsValid; ws;
               -- (trace ("\n\nisValid: " ++ (show isValid)) ws);
               possMoves <- try parsePossMoves <|> nilPM; ws; 
               -- (trace ("\n\npossmoves: " ++ (show possMoves)) ws);
               outcome <- parseOutcome; ws;
               -- (trace ("\n\noutcome: " ++ (show outcome)) ws);
               initState <- parseInitState gamestate; ws;
               -- (trace ("init: " ++ (show initState)) ws);
               fromString <- parseFromString; ws;
               -- (trace ("\n\nfromstring: " ++ (show fromString)) ws);
               customData <- parseCustomData; ws;
               -- (trace ("custData" ++ (show customData)) ws);
               return (Game (id, gamestate, move, isValid, possMoves, outcome, 
                            initState, player, fromString, customData, imports));
             }}

parseCustomData :: Parser [Dec]
parseCustomData = do {
                    -- (trace "parsing custdata" ws);
                    -- name <- many (noneOf ": ");
                    ws;
                    m_reservedOp "$"; ws;
                    decs <- many (noneOf "$");
                    -- dataDec <- decParser name;
                    case parseDecs decs of
                      (Left err) -> do {return (trace ("failed:\n" ++ err) undefined)}
                      (Right d) -> do {return d}
                    -- return (CustomDataType dataDec);
                  }

parseImports :: Parser [Dec]
parseImports = do {
                 ws; 
                 m_reserved "import"; ws;
                 file <- many (noneOf " \n\r\t"); ws;
                 -- (trace ("filename: " ++ file) ws);
                 decs <- (parseDecsFromFile (file ++ ".hs"));
                 -- (trace ("importDecs: " ++ (show decs)) ws);
                 -- moredecs <- try parseImports;
                 -- (trace "moreDecs..." ws);
                 -- return (decs ++ moredecs);
                 return decs;
               }

parseDecsFromFile :: String -> Parser [Dec]
parseDecsFromFile filename = 
      let contents = unsafePerformIO (readFile filename) in
      case parseDecs contents of
        (Left err) -> do {return (trace err undefined)}
        (Right decs) -> do {return decs}
    

parsePlayer :: Parser Player
parsePlayer = do {
                ws;
                m_reserved "Player";
                ws;
                m_reservedOp ":";
                ws;
                playerDec <- playerDecParser "Player";
                return (Player playerDec);
              }
playerDecParser :: String -> Parser (Dec, Int)
playerDecParser str = do {
              ws;
              m_reservedOp "{";
              dec <- (many (noneOf "}"));
              let nplayers = (length (filter ((==) '|') dec)) + 1 in do {
              m_reservedOp "}";
              case parseDecs ("data " ++ str ++ " = " ++ dec ++ " deriving (Eq, Show)") of
                (Right d) -> do {return ((head d), nplayers)}
                (Left err) -> case parseDecs ("data " ++ str ++ " = " ++ str ++ "(" ++ dec ++ ")" ++ " deriving (Eq, Show)") of
                                (Right d) -> do {return ((head d), nplayers)}
                                (Left err) -> do {trace "decParser" (return undefined)}
                }}


parseFromString :: Parser FromString
parseFromString = do {
                ws;
                m_reserved "fromString";
                ws;
                m_reservedOp ":";
                ws;
                e <- simpleExpParser "String -> Move";
                return (FromString e);
              }

parsePossMoves :: Parser PossMovesFun
parsePossMoves = do {
                   ws;
                   m_reserved "possMoves"; ws;
                   m_reservedOp ":"; ws;
                   possFun <- expParser "[Move]";
                   return (PossMovesFun possFun);
                 }
                   
parseOutcome :: Parser OutcomeFun
parseOutcome =  do {
                  ws;
                  m_reserved "outcome"; ws;
                  m_reservedOp ":"; ws;
                  (try parseCustomOutcome) <|> parseOutcome';
                }

parseCustomOutcome :: Parser OutcomeFun
parseCustomOutcome = do {
                       m_reservedOp "<<"; ws;
                       e <- (manyTill anyChar (try (string ">>")));
                          -- m_reservedOp ">>";
                       case parseExp ("(\\game -> " ++ e ++ ")") of
                         (Left err) -> do {trace ("error1: " ++ e ++ "\nerror: " ++ err) (return undefined)}
                         (Right exp) -> do {return (CustOutcomeFun exp)}}

parseOutcome' :: Parser OutcomeFun
parseOutcome' = do {
                  ws;
                  m_reservedOp "{";
                  winc <- winConditionParser;
                  tiec <- tieConditionParser;
                  elsec <- elseConditionParser;
                  m_reservedOp "}";
                  return (OutcomeFun {wincon=winc, tiecon=tiec, elsecon=elsec})}

winConditionParser :: Parser Exp
winConditionParser = do {
                       ws;
                       m_reserved "winCondition"; ws;
                       m_reservedOp ":"; 
                       e <- simpleExpParser "";
                       return e;
                     }

tieConditionParser :: Parser Exp
tieConditionParser = do {
                       ws;
                       m_reserved "tieCondition"; ws;
                       m_reservedOp ":"; ws;
                       e <- simpleExpParser "";
                       return e;
                     }

elseConditionParser :: Parser Exp
elseConditionParser = do {
                        ws;
                        m_reserved "else"; ws;
                        m_reservedOp ":"; ws;
                        e <- simpleExpParser "";
                        return e;
                      }


parseIsValid :: Parser IsValidFun
parseIsValid = do {
                   ws;
                   m_reserved "isValid"; ws;
                   m_reservedOp ":"; ws;
                   isValidF <- expParser "Move -> Bool";
                   return (IsValidFun isValidF);
                 }

parseInitState :: GameState -> Parser InitState
parseInitState gs = do {
                   ws;
                   m_reserved "initialState"; ws;
                   m_reservedOp ":"; ws;
                   m_reservedOp "{"; ws;
                   m_reserved "Board"; ws;
                   m_reservedOp ":"; ws;
                   boardDec <- parseInitBoard (board gs); ws;
                   m_reserved "Turn"; ws;
                   m_reservedOp ":"; ws;
                   turnDec <- simpleExpParser "Player"; ws;
                   m_reservedOp "}";
                   --  initState <- expParser;                 
                   -- return (InitState initState); 
                   return InitState{boardInit=boardDec, turnInit=turnDec};
                 }


parseInitBoard :: Board -> Parser Exp
parseInitBoard board = try (parseBoardAll board) <|>
                       try (parseBoardLit board) <|> 
                       parseCustomBoard

parseCustomBoard :: Parser Exp 
parseCustomBoard = do {
                          ws; m_reservedOp "<<";
                          e <- (manyTill anyChar (try (string ">>")));
                          -- m_reservedOp ">>";
                          case parseExp e {-++ " :: " ++ typeS-} of
                            (Left err) -> do {trace ("error1 " ++ e ++ "\nerror: " ++ err) (return undefined)}
                            (Right exp) -> do {return exp}
                        }               

parseBoardAll :: Board -> Parser Exp
parseBoardAll (Matrix (d, (i, j))) = let s = "matrix " ++ (show i) ++ " " ++ 
                                             (show j) ++ " (\\(i,j) -> "
                                     in do {
                                          ws; m_reservedOp "{"; ws;
                                          m_reserved "all"; ws;
                                          piece <- (many (noneOf "}"));
                                          m_reservedOp "}";
                                          case parseExp (s ++ piece ++ ")") of
                                            (Right e) -> do {return e}
                                            (Left err) -> do {(trace ("error: " ++ err)(return undefined))}}
parseBoardAll (Array (d, i)) = let s = "generate " ++ (show i) ++ " (\\i -> "
                               in do {
                                    ws; m_reservedOp "{"; ws;
                                    m_reserved "all"; ws;
                                    piece <- (many (noneOf "}"));
                                    m_reservedOp "}";
                                    case parseExp (s ++ piece ++ ")") of
                                      (Right e) -> do {return e}
                                      (Left err) -> do {return undefined}}
parseBoardAll _ = do {m_reserved "ERROR"; return (VarE (mkName "nil"));}

parseBoardLit :: Board -> Parser Exp
parseBoardLit (Matrix _) = do {
                              ws; m_reservedOp "{"; ws;
                              lists <- (many (noneOf "}"));
                              m_reservedOp "}";
                              case parseExp ("M.fromLists " ++ lists) of
                                (Right e) -> do {return e}
                                (Left err) -> do {return undefined}}
parseBoardLit (Array _) = do {
                              ws; m_reservedOp "{"; ws;
                              list <- (many (noneOf "}"));
                              m_reservedOp "}";
                              case parseExp ("V.fromList " ++ list) of
                                (Right e) -> do {return e}
                                (Left err) -> do {return undefined}}
parseBoardLit _ = do {m_reserved "ERROR"; return (VarE (mkName "nil"))}

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

boardTypeParser :: String -> Parser Board
boardTypeParser name = do {
              ws;
              m_reservedOp "<<";
              ty <- (manyTill anyChar (try (string ">>")));
              -- m_reservedOp ">";
              case parseDecs ("type " ++ name ++ " = " ++ ty) of
                (Left err) -> do {trace "typeParser" (return undefined)}
                (Right typ) -> do {return (Board (head typ))}
            } 

simpleExpParser :: String -> Parser Exp
simpleExpParser typeS = do {
                          ws; m_reservedOp "{";
                          e <- (many (noneOf "}"));
                          m_reservedOp "}";
                          case parseExp e {-++ " :: " ++ typeS-} of
                            (Left err) -> do {trace ("expParser: " ++ err) (return undefined)}
                            (Right exp) -> do {return exp}
                        }               

sParser :: Parser String
sParser = do {
            s <- (many (noneOf "{}"));
            -- (trace ("parsed: " ++ s)
            try (do {
                   m_reservedOp "{";
                   nested <- sParser;
                   m_reservedOp "}";
                   rest <- sParser;
                   return (s ++ "{" ++ nested ++ "}" ++ rest);
                 }) <|>
            -- (trace ("returning s") (return s));
                return s;
          }


expParser :: String -> Parser Exp
expParser "" = do {
                 ws;
                 m_reservedOp "{";
                 -- e <- (many (noneOf "}"));
                 e <- sParser;
                 m_reservedOp "}";
                 case parseExp ("(\\game -> " ++ e ++ ")") of
                   (Left err) -> do {trace ("expParser: " ++ e ++ "\n error: " ++err) (return undefined)}
                   (Right exp) -> do {return exp}
            }
                 
expParser typeS = do {
              ws;
              m_reservedOp "{";
              e <- sParser;
              -- e <- (many (noneOf "}"));
              m_reservedOp "}";
              --trace ("parsing from: " ++ "(\\game -> " ++ e ++ ") :: Gamestate -> (Int, Int) -> Int") ws;
              case parseExp ("(\\game -> " ++ e ++ ") :: GameState -> " ++ typeS) of
                (Left err) -> do {trace ("expParser: " ++ err) (return undefined)}
                (Right exp) -> do {return exp}
            }
    
decParser :: String -> Parser Dec
decParser str = do {
              ws;
              m_reservedOp "{";
              dec <- (many (noneOf "}"));
              m_reservedOp "}";
              case parseDecs ("data " ++ str ++ " = " ++ dec ++ " deriving (Eq, Show)") of
                (Right d) -> do {return (head d)}
                (Left err) -> case parseDecs ("data " ++ str ++ " = " ++ str ++ "(" ++ dec ++ ")" ++ " deriving (Eq, Show)") of
                                (Right d) -> do {return (head d)}
                                (Left err) -> do {trace "decParser" (return undefined)}
                }
     
simpleDecParser :: String -> Parser Dec
simpleDecParser str = do {
              ws;
              m_reservedOp "<<";
              dec <- (manyTill anyChar (try (string ">>")));
              -- m_reservedOp "<";
              case parseDecs ("data " ++ str ++ " = " ++ dec ++ " deriving (Eq, Show)") of
                (Right d) -> do {return (head d)}
                (Left err) -> case parseDecs ("data " ++ str ++ " = " ++ str ++ "(" ++ dec ++ ")" ++ " deriving (Eq, Show)") of
                                (Right d) -> do {return (head d)}
                                (Left err) -> do {trace "simpledecParser" (return undefined)}
                }

pieceDecParser :: Parser Dec
pieceDecParser = do {
              ws;
              m_reservedOp "{";
              dec <- (many (noneOf "}"));
              m_reservedOp "}";
              case parseDecs ("data Piece = " ++ dec ++ "| Nil deriving (Eq, Show)") of
                (Right d) -> do {return (head d)}
                (Left err) -> case parseDecs ("data Piece = Piece(" ++ dec ++ ")" ++ "| Nil deriving (Eq, Show)") of
                                (Right d) -> do {return (head d)}
                                (Left err) -> do {trace "decParser" (return undefined)}
                }


parseSize :: Parser (Integer, Integer)
parseSize = do {
              n <- m_brackets m_natural; ws;
              m <- m_brackets m_natural; ws;
              return (n,m);
            }

matrixParser :: Parser Board 
matrixParser = do {
                ws;
                m_reservedOp "{";ws;
                m_reserved "Matrix";ws;
                size <- parseSize;ws;
                m_reservedOp "}";ws;
                
                case  parseDecs ("type Board = Matrix Piece") of
                  (Right d) -> do {return (Matrix ((head d), size))}
                  (Left err) -> do {trace "matrixParser" (return undefined)} 
               }

arrayParser :: Parser Board 
arrayParser = do {
                ws;
                m_reservedOp "{";ws;
                m_reserved "Array";ws;
                size <- m_brackets m_natural;ws;
                m_reservedOp "}";ws;
                
                case  parseDecs ("type Board = [Piece]") of
                  (Right d) -> do {return (Array ((head d), size))}
                  (Left err) -> do {trace "matrixParser" (return undefined)} 
               }


boardParser :: Parser Board
boardParser = do {
                ws;
                m_reserved "Board"; ws;
                m_reservedOp ":"; ws;
                t <- (try matrixParser <|> 
                      try arrayParser <|> 
                          boardTypeParser "Board");
                return t;
}

pieceParser :: Parser Dec
pieceParser = do {
                ws;
                m_reserved "Piece"; ws;
                m_reservedOp ":"; ws;
                t <- pieceDecParser;
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

-- customDParser :: Parser Dec
-- customDParser = do {
--                 ws;
--                 m_reserved "NOTSUPPORTED"; ws;
--                 m_reservedOp ":"; ws;
--                 t <- (try matrixParser <|> decParser "Board");
--                 return t;
-- }

nilParser :: Parser Dec
nilParser = case parseDecs "data NULL = NULL" of
              (Right d) -> do {return (head d)}
              (Left err) -> do {trace "nilParser" (return undefined)}

nilPM :: Parser PossMovesFun
nilPM = do {return PMNil} -- case parseExp "nil" of
        --    (Right e) -> do {return (PossMovesFun e)}
        --    (Left err) -> do {trace "nilExp" (return undefined)}

parseGameState :: Parser GameState 
parseGameState = do {
                   btypedec <- boardParser;-- <|> nilParser;
                   nilT <- nilParser;
                   ptypedec <- pieceParser <|> nilParser;
                   htypedec <- handParser <|> nilParser;
                   ttypedec <- turnParser <|> nilParser;
                   -- ctypedec <- customDParser <|> nilParser;
                   return GameState 
                           {board=btypedec, piece=ptypedec,
                            hand=htypedec, turn=ttypedec};}
                 
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

parseGame :: String -> Game
parseGame input = case parse gameParser "" input of
                    Left err -> (trace "parseGameFail" NIL)
                    Right ans ->  ans

parseToS :: String -> String
parseToS input = case parse gameParser "" input of
                (Left err) -> (show NIL)
                (Right ans) -> (show ans)
                    
ws = m_whiteSpace;

