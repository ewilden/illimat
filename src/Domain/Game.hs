module Domain.Game where

import           ClassyPrelude
import qualified Domain.Auth as D.Auth
import qualified GameLogic as GL

type GameId = Text

data Game = Game 
  { _gameId :: GameId
  , _gameData :: GameData
  , _gamePlayers :: [D.Auth.UserId]
  } deriving (Show)

data GameData =
  GameDataRunning RunningGame
  | GameDataStarting StartingGame
  | GameDataFinished FinishedGame
  deriving (Show)

data GameView = GameView
  { _gvGameId :: GameId
  , _gvData :: GameViewData
  , _gvPlayers :: [D.Auth.UserId]
  } deriving (Show, Eq)

computeViewForPlayer :: GL.PlayerIndex -> Game -> GameView
computeViewForPlayer = error "computeViewForPlayer: not implemented yet"

data GameViewData =
  GameViewRunning GL.GameStateView
  | GameViewStarting StartingGame
  | GameViewFinished FinishedGame
  deriving (Show, Eq)

data RunningGame = RunningGame
  { _rgGameState :: GL.GameState 
  } deriving (Show)

data StartingGame = StartingGame 
  { _sgMaxNumPlayers :: Int 
  } deriving (Show, Eq)

data FinishedGame 
  = FinishedGame
  deriving (Show, Eq)

data CreateGameRequest = CreateGameRequest
  { _cgreqGameOwner :: D.Auth.UserId
  , _cgreqMaxNumPlayers :: Int
  } deriving (Show, Eq)

data CreateGameResponse = CreateGameResponse
  { _cgrespGameView :: GameView
  } deriving (Show)

data CreateGameError
  = CreateGameErrorUnsupportedNumPlayers
  deriving (Show, Eq)

data JoinGameError 
  = JoinGameErrorTooManyPlayers
  | JoinGameErrorAlreadyInGame
  | JoinGameErrorNoSuchGame
  | JoinGameErrorGameAlreadyRunning
  | JoinGameErrorGameAlreadyFinished
  deriving (Show, Eq)

data JoinGameRequest = JoinGameRequest
  { _jgreqUserId :: D.Auth.UserId 
  , _jgreqGameId :: GameId 
  } deriving (Show, Eq)

data JoinGameResponse = JoinGameResponse
  { _jgrespGameView :: GameView
  } deriving (Show)

data MakeMoveError 
  = MakeMoveErrorInvalidMove Text
  deriving (Show, Eq)

data MakeMoveRequest = MakeMoveRequest
  { _mmreqUserId :: D.Auth.UserId
  , _mmreqGameId :: GameId
  , _mmreqMove :: GL.Move
  } deriving (Show, Eq)

data MakeMoveResponse = MakeMoveResponse
  { _mmrespGameStateView :: GameView
  } deriving (Show, Eq)

data GetGameViewRequest = GetGameViewRequest
  { _ggvreqUserId :: D.Auth.UserId
  , _ggvreqGameId :: GameId
  } deriving (Show, Eq)

data GetGameViewResponse = GetGameViewResponse
  { _ggvrespGameView :: GameView
  } deriving (Show, Eq)

data GetGameViewError =
  GetGameViewErrorNoSuchGame
  | GetGameViewErrorUserNotInGame
  deriving (Show, Eq)

data GetGamesForUserRequest = GetGamesForUserRequest
  { _ggfureqUserId :: D.Auth.UserId
  } deriving (Show, Eq)

data GetGamesForUserResponse = GetGamesForUserResponse
  { _ggfurespGameIds :: [GameId]
  } deriving (Show, Eq)

class Monad m => GameRepo m where
  createGame :: CreateGameRequest -> m (Either CreateGameError CreateGameResponse)
  joinGame :: JoinGameRequest -> m (Either JoinGameError JoinGameResponse)
  makeMove :: MakeMoveRequest -> m (Either MakeMoveError MakeMoveResponse)
  getGameView :: GetGameViewRequest -> m (Either GetGameViewError GetGameViewResponse)
  getGamesForUser :: GetGamesForUserRequest -> m (GetGamesForUserResponse)
