module Domain.Game where

import           ClassyPrelude
import Domain.Auth as D.Auth
import GameLogic as GL

type GameId = Text

data CreateGameRequest = CreateGameRequest
  { _cgreqGameOwner :: UserId
  , _cgreqMaxNumPlayers :: Int
  } deriving (Show, Eq)

data CreateGameResponse = CreateGameResponse
  { _cgrespGameId :: GameId 
  } deriving (Show, Eq)

data CreateGameError
  = CreateGameErrorUnsupportedNumPlayers
  deriving (Show, Eq)

data JoinGameError 
  = JoinGameErrorTooManyPlayers
  deriving (Show, Eq)

data JoinGameRequest = JoinGameRequest
  { _jgreqUserId :: UserId 
  , _jgreqGameId :: GameId 
  } deriving (Show, Eq)

data JoinGameResponse = JoinGameResponse
  { 
  } deriving (Show, Eq)

data MakeMoveError 
  = MakeMoveErrorInvalidMove Text
  deriving (Show, Eq)

data MakeMoveRequest = MakeMoveRequest
  { _mmreqUserId :: UserId
  , _mmreqGameId :: GameId
  , _mmreqMove :: GL.Move
  } deriving (Show, Eq)

data MakeMoveResponse = MakeMoveResponse
  { _mmrespGameStateView :: GL.GameStateView
  } deriving (Show, Eq)

data GetGameViewRequest = GetGameViewRequest
  { _ggvreqUserId :: UserId
  , _ggvreqGameId :: GameId
  } deriving (Show, Eq)

data GetGameViewResponse = GetGameViewResponse
  { _ggvrespGameStateView :: GL.GameStateView
  } deriving (Show, Eq)

data GetGameViewError =
  GetGameViewErrorNoSuchGame
  | GetGameViewErrorNoSuchUser
  | GetGameViewErrorUserNotInGame
  deriving (Show, Eq)

data GetGamesForUserRequest = GetGamesForUserRequest
  { _ggfureqUserId :: UserId
  } deriving (Show, Eq)

data GetGamesForUserResponse = GetGamesForUserResponse
  { _ggfurespGameIds :: [GameId]
  } deriving (Show, Eq)

data GetGamesForUserError
  = GetGamesForUserErrorNoSuchUser
  deriving (Show, Eq)

class Monad m => GameRepo m where
  createGame :: CreateGameRequest -> m (Either CreateGameError CreateGameResponse)
  joinGame :: JoinGameRequest -> m (Either JoinGameError JoinGameResponse)
  makeMove :: MakeMoveRequest -> m (Either MakeMoveError MakeMoveResponse)
  getGameView :: GetGameViewRequest -> m (Either GetGameViewError GetGameViewResponse)
  getGamesForUser :: GetGamesForUserRequest -> m (Either GetGamesForUserError GetGamesForUserResponse)
