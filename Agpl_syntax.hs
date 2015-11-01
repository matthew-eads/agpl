module Agpl_syntax where
import Language.Haskell.TH.Syntax

data Game = Game ([GameState], Move, IsValidFun, PossMovesFun, OutcomeFun, InitState,
                  Player, [CustomDataType]) | NIL | GSL ([GameState]) deriving Show

data GameState = Board (Type) | Piece (Type) | Hand (Type) | Turn (Type) |
                 CustomData (Type) | None deriving Show

type GameID = String
data Move = Move (Type) deriving Show
data Player = Player (Type) deriving Show
data IsValidFun = IsValidFun (Exp) deriving Show
data PossMovesFun = PossMovesFun (Exp) deriving Show
data OutcomeFun = OutcomeFun (Exp) deriving Show
data CustomDataType = CustomDataType (Type) deriving Show
data InitState = InitState (Exp) deriving Show
