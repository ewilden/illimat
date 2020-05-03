module Domain.Game where

import           RIO
import qualified System.Random as Random

import qualified Domain.Game.GameLogic as GL
import qualified Prelude ((!!))

type GameId = Text
type UserId = Text

data Game = Game
  { _gameId :: !GameId
  , _gameData :: !GameData
  , _gamePlayers :: ![UserId]
  } deriving (Show)

data GameData
  = GameDataRunning RunningGame
  | GameDataStarting StartingGame
  | GameDataFinished FinishedGame
  deriving (Show)

data GameView = GameView
  { _gvData :: !GameViewData
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
  { _rgGameState :: !GL.GameState
  } deriving (Show)

data StartingGame = StartingGame deriving (Show, Eq)

data FinishedGame
  = FinishedGame
  deriving (Show, Eq)

data CreateGameRequest = CreateGameRequest
  { _cgreqGameOwner :: !UserId
  } deriving (Show, Eq)

data CreateGameResponse = CreateGameResponse
  { _cgrespGameView :: !GameView
  , _cgrespGameId :: !GameId
  } deriving (Show)

data JoinGameError
  = JoinGameErrorAlreadyInGame
  | JoinGameErrorNoSuchGame
  | JoinGameErrorGameAlreadyRunning
  | JoinGameErrorGameAlreadyFinished
  deriving (Show, Eq)

data JoinGameRequest = JoinGameRequest
  { _jgreqUserId :: !UserId
  , _jgreqGameId :: !GameId
  } deriving (Show, Eq)

data JoinGameResponse = JoinGameResponse
  { _jgrespGameView :: !GameView
  } deriving (Show)

data StartGameRequest = StartGameRequest
    { _sgreqUserId :: !UserId
    , _sgreqGameId :: !GameId
    } deriving (Show, Eq)

data StartGameResponse = StartGameResponse
    { _sgrespGameView :: !GameView
    } deriving (Show)

data StartGameError 
    = StartGameErrorNotGameOwner
    | StartGameErrorNotEnoughPlayers
    | StartGameErrorNoSuchGame
    | StartGameErrorNotInGame
    | StartGameErrorGameAlreadyStarted
    | StartGameErrorOther Text
    deriving (Show)

data MakeMoveError
  = MakeMoveErrorInvalidMove Text
  | MakeMoveErrorNoSuchGame
  | MakeMoveErrorPlayerNotInGame
  | MakeMoveErrorGameNotStartedYet
  | MakeMoveErrorGameAlreadyFinished
  deriving (Show, Eq)

data MakeMoveRequest = MakeMoveRequest
  { _mmreqUserId :: !UserId
  , _mmreqGameId :: !GameId
  , _mmreqMove :: !GL.Move
  } deriving (Show, Eq)

data MakeMoveResponse = MakeMoveResponse
  { _mmrespGameStateView :: !GameView
  } deriving (Show, Eq)

data GetGameViewRequest = GetGameViewRequest
  { _ggvreqUserId :: !UserId
  , _ggvreqGameId :: !GameId
  } deriving (Show, Eq)

data GetGameViewResponse = GetGameViewResponse
  { _ggvrespGameView :: !GameView
  } deriving (Show, Eq)

data GetGameViewError
  = GetGameViewErrorNoSuchGame
  | GetGameViewErrorUserNotInGame
  deriving (Show, Eq)

data GetGamesForUserRequest = GetGamesForUserRequest
  { _ggfureqUserId :: !UserId
  } deriving (Show, Eq)

data GetGamesForUserResponse = GetGamesForUserResponse
  { _ggfurespGameIds :: ![GameId]
  } deriving (Show, Eq)

class Monad m => GameRepo m where
  createGame :: CreateGameRequest -> m CreateGameResponse
  joinGame :: JoinGameRequest -> m (Either JoinGameError JoinGameResponse)
  makeMove :: MakeMoveRequest -> m (Either MakeMoveError MakeMoveResponse)
  startGame :: StartGameRequest -> m (Either StartGameError StartGameResponse)
  getGameView :: GetGameViewRequest -> m (Either GetGameViewError GetGameViewResponse)
  getGamesForUser :: GetGamesForUserRequest -> m GetGamesForUserResponse

shuffle :: (Random.RandomGen g) => [a] -> g -> ([a], g)
shuffle x g = if length x < 2 then (x, g) else
  let
    (i, g') = Random.randomR (0, length(x) - 1) g
    (r, g'') = shuffle (take i x ++ drop (i+1) x) g'
  in
    (x Prelude.!! i : r, g'')

initEmptyGameState :: (Random.RandomGen g) => Int -> g -> (GL.GameState, g)
initEmptyGameState numPlayers g0 =
  let shuffleCardsFor n g
        | n >= 4 = shuffle GL.allCards g
        | otherwise = shuffle GL.allCardsMinusStars g
      (shuffledDeck, g1) = shuffleCardsFor numPlayers g0
      (shuffledLums, g2) = shuffle GL.allLuminaries g1
      (shuffledDirs, g3) = shuffle (GL.allEnum :: [GL.Direction]) g2
  in
    (GL.emptyGameState numPlayers (shuffledDirs Prelude.!! 0) shuffledDeck shuffledLums, g3)
