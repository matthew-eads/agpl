module Agpl_syntax where
import Language.Haskell.TH.Syntax

data Game = Game ([GameState], Move, IsValidFun, PossMovesFun, OutcomeFun, InitState,
                  Player, [CustomDataType]) deriving Show

data GameState = Board (Dec) | Piece (Dec) | Hand (Dec) | Turn (Dec) |
                 CustomData (Dec) deriving Show

data Move = Move (Dec) deriving Show
data Player = Player (Dec) deriving Show
data IsValidFun = IsValidFun (Exp) deriving Show
data PossMovesFun = PossMovesFun (Exp) deriving Show
data OutcomeFun = OutcomeFun (Exp) deriving Show
data CustomDataType = CustomDataType (Dec) deriving Show
data InitState = InitState (Exp) deriving Show
