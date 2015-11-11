module Agpl_syntax where
import Language.Haskell.TH.Syntax

data Game = Game (GameID, GameState, Move, IsValidFun, PossMovesFun, 
                  OutcomeFun, InitState, Player, [Dec]) 
          | NIL deriving Show


data GameState = GameState {board :: Dec, piece :: Dec, hand :: Dec, turn :: Dec, customData :: Dec}
                 deriving Show

type GameID = String
data Move = Move (Dec) deriving Show
data Player = Player (Dec) deriving Show
data IsValidFun = IsValidFun (Exp) deriving Show
data PossMovesFun = PossMovesFun (Exp) deriving Show
data OutcomeFun = OutcomeFun (Exp) deriving Show
data CustomDataType = CustomDataType (Dec) deriving Show
-- data InitState = InitState (Exp) deriving Show
data InitState = InitState {boardInit :: Exp, turnInit :: Exp} deriving Show
