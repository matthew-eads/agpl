module Agpl_syntax where
import Language.Haskell.TH.Syntax

data Game = Game (GameID, GameState, Move, IsValidFun, PossMovesFun, 
                  OutcomeFun, InitState, Player, FromString, [Dec], [Dec]) 
          | NIL deriving Show


data GameState = GameState {board :: Board, piece :: Dec, hand :: Dec, turn :: Dec}
                 deriving Show
data Board = Matrix (Dec, (Integer, Integer))
           | Array (Dec, Integer)
           | Board (Dec) deriving Show

data OutcomeFun = CustOutcomeFun (Exp) 
                | OutcomeFun {wincon :: Exp, tiecon :: Exp, elsecon :: Exp} deriving Show

type GameID = String
data Move = Move (Dec) deriving Show
data Player = Player (Dec, Int) deriving Show
data IsValidFun = IsValidFun (Exp) deriving Show
data PossMovesFun = PossMovesFun (Exp) | PMNil deriving Show
-- data OutcomeFun = OutcomeFun (Exp) deriving Show
data FromString = FromString (Exp) deriving Show
data CustomDataType = CustomDataType (Dec) deriving Show
-- data InitState = InitState (Exp) deriving Show
data InitState = InitState {boardInit :: Exp, turnInit :: Exp} deriving Show
