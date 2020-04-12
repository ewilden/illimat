module Domain.Game where

import           ClassyPrelude
import qualified System.Random as Random

import qualified Domain.Auth as D.Auth
import qualified GameLogic as GL
import qualified Prelude as Prelude ((!!))

type GameId = Text

data Game = Game 
  { _gameId :: GameId
  , _gameData :: GameData
  , _gamePlayers :: [D.Auth.UserId]
  } deriving (Show)

data GameData 
  = GameDataRunning RunningGame
  | GameDataStarting StartingGame
  | GameDataFinished FinishedGame
  deriving (Show)

data GameView = GameView
  { _gvData :: GameViewData
  } deriving (Show, Eq)

computeViewForPlayer :: GL.PlayerIndex -> Game -> GameView
computeViewForPlayer playerIndex (Game gid gameData players) = 
  GameView $ case gameData of
    GameDataStarting g -> GameViewStarting g
    GameDataFinished g -> GameViewFinished g
    GameDataRunning (RunningGame gameState) -> 
      GameViewRunning $ GL.computeViewForPlayer playerIndex gameState

data GameViewData 
  = GameViewRunning GL.GameStateView
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
  , _cgrespGameId :: GameId
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
  | MakeMoveErrorNoSuchGame
  | MakeMoveErrorPlayerNotInGame
  | MakeMoveErrorGameNotStartedYet
  | MakeMoveErrorGameAlreadyFinished
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

data GetGameViewError 
  = GetGameViewErrorNoSuchGame
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

shuffle :: (Random.RandomGen g) => [a] -> g -> ([a], g)
shuffle x g = if length x < 2 then (x, g) else 
  let
    (i, g') = Random.randomR (0, length(x) - 1) g
    (r, g'') = shuffle (take i x ++ drop (i+1) x) g'
  in
    ((x Prelude.!! i : r), g'')

initEmptyGameState :: (Random.RandomGen g) => Int -> g -> (GL.GameState, g)
initEmptyGameState numPlayers g0 =
  let shuffleCardsFor n g
        | n >= 4 = shuffle GL.allCards g
        | otherwise = shuffle GL.allCardsMinusStars g
      (shuffledDeck, g1) = shuffleCardsFor numPlayers g0
      (shuffledLums, g2) = shuffle GL.allLuminaries g1
      (shuffledDirs, g3) = shuffle (GL.allEnum :: [GL.Direction]) g2
  in 
    ((GL.emptyGameState numPlayers (shuffledDirs Prelude.!! 0) shuffledDeck shuffledLums), g3)
