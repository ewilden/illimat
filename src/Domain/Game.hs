{-# LANGUAGE TemplateHaskell #-}

module Domain.Game where

import           Data.Aeson.TH                  ( )
import           Data.Aeson.TypeScript.TH
import qualified Prelude                        ( (!!) )
import           RIO
import qualified System.Random                 as Random

import qualified AesonOptions
import qualified Domain.Game.GameLogic         as GL
import qualified Domain.User                   as U


type GameId = Text
type UserId = U.UserId


data FinishedGame
  = FinishedGame
  deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''FinishedGame)

data GameData
  = GameDataRunning RunningGame
  | GameDataStarting StartingGame
  | GameDataFinished FinishedGame
  deriving (Show)

data Game = Game
  { _gameId :: !GameId
  , _gameData :: !GameData
  , _gamePlayers :: ![UserId]
  } deriving (Show)

data RunningGame = RunningGame
  { _rgGameState :: !GL.GameState
  } deriving (Show)

data StartingGame = StartingGame deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''StartingGame)

data GameViewData
  = GameViewRunning GL.GameStateView
  | GameViewStarting StartingGame
  | GameViewFinished FinishedGame
  deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''GameViewData)

data GameView = GameView
  { _gvData :: !GameViewData
  } deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''GameView)

computeViewForPlayer :: GL.PlayerIndex -> Game -> GameView
computeViewForPlayer playerIndex (Game gid gameData players) =
  GameView $ case gameData of
    GameDataStarting g -> GameViewStarting g
    GameDataFinished g -> GameViewFinished g
    GameDataRunning (RunningGame gameState) ->
      GameViewRunning $ GL.computeViewForPlayer playerIndex gameState

data CreateGameRequest = CreateGameRequest
  { _cgreqGameOwner :: !UserId
  } deriving (Show, Eq)

data CreateGameResponse = CreateGameResponse
  { _cgrespGameView :: !GameView
  , _cgrespGameId :: !GameId
  , _cgrespUserId :: !UserId
  } deriving (Show)

$(deriveJSONAndTypeScript AesonOptions.options ''CreateGameResponse)

data JoinGameError
  = JoinGameErrorAlreadyInGame
  | JoinGameErrorNoSuchGame
  | JoinGameErrorGameAlreadyRunning
  | JoinGameErrorGameAlreadyFinished
  deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''JoinGameError)

data JoinGameRequest = JoinGameRequest
  { _jgreqUserId :: !UserId
  , _jgreqGameId :: !GameId
  } deriving (Show, Eq)

data JoinGameResponse = JoinGameResponse
  { _jgrespGameView :: !GameView
  , _jgrespUserId :: !UserId
  } deriving (Show)
$(deriveJSONAndTypeScript AesonOptions.options ''JoinGameResponse)

data StartGameRequest = StartGameRequest
    { _sgreqUserId :: !UserId
    , _sgreqGameId :: !GameId
    } deriving (Show, Eq)

data StartGameResponse = StartGameResponse
    { _sgrespGameView :: !GameView
    , _sgrespUserId :: !UserId
    } deriving (Show)
$(deriveJSONAndTypeScript AesonOptions.options ''StartGameResponse)

data StartGameError
    = StartGameErrorNotGameOwner
    | StartGameErrorNotEnoughPlayers
    | StartGameErrorNoSuchGame
    | StartGameErrorNotInGame
    | StartGameErrorGameAlreadyStarted
    | StartGameErrorOther Text
    deriving (Show)
$(deriveJSONAndTypeScript AesonOptions.options ''StartGameError)

data MakeMoveError
  = MakeMoveErrorInvalidMove Text
  | MakeMoveErrorNoSuchGame
  | MakeMoveErrorPlayerNotInGame
  | MakeMoveErrorGameNotStartedYet
  | MakeMoveErrorGameAlreadyFinished
  deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''MakeMoveError)

data MakeMoveRequest = MakeMoveRequest
  { _mmreqUserId :: !UserId
  , _mmreqGameId :: !GameId
  , _mmreqMove :: !GL.Move
  } deriving (Show, Eq)

data MakeMoveResponse = MakeMoveResponse
  { _mmrespGameStateView :: !GameView
  , _mmrespUserId :: !UserId
  } deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''MakeMoveResponse)

data GetGameViewRequest = GetGameViewRequest
  { _ggvreqUserId :: !UserId
  , _ggvreqGameId :: !GameId
  } deriving (Show, Eq)

data GetGameViewResponse = GetGameViewResponse
  { _ggvrespGameView :: !GameView
  , _ggvrespUserId :: !UserId
  } deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''GetGameViewResponse)

data GetGameViewError
  = GetGameViewErrorNoSuchGame
  | GetGameViewErrorUserNotInGame
  deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''GetGameViewError)

data GetGamesForUserRequest = GetGamesForUserRequest
  { _ggfureqUserId :: !UserId
  } deriving (Show, Eq)

data GetGamesForUserResponse = GetGamesForUserResponse
  { _ggfurespGameIds :: ![GameId]
  , _ggfurespUserId :: !UserId
  } deriving (Show, Eq)
$(deriveJSONAndTypeScript AesonOptions.options ''GetGamesForUserResponse)

class Monad m => GameRepo m where
  createGame :: CreateGameRequest -> m CreateGameResponse
  joinGame :: JoinGameRequest -> m (Either JoinGameError JoinGameResponse)
  makeMove :: MakeMoveRequest -> m (Either MakeMoveError MakeMoveResponse)
  startGame :: StartGameRequest -> m (Either StartGameError StartGameResponse)
  getGameView :: GetGameViewRequest -> m (Either GetGameViewError GetGameViewResponse)
  getGamesForUser :: GetGamesForUserRequest -> m GetGamesForUserResponse

shuffle :: (Random.RandomGen g) => [a] -> g -> ([a], g)
shuffle x g = if length x < 2
  then (x, g)
  else
    let (i, g' ) = Random.randomR (0, length (x) - 1) g
        (r, g'') = shuffle (take i x ++ drop (i + 1) x) g'
    in  (x Prelude.!! i : r, g'')

initEmptyGameState :: (Random.RandomGen g) => Int -> g -> (GL.GameState, g)
initEmptyGameState numPlayers g0 =
  let shuffleCardsFor n g | n >= 4    = shuffle GL.allCards g
                          | otherwise = shuffle GL.allCardsMinusStars g
      (shuffledDeck, g1) = shuffleCardsFor numPlayers g0
      (shuffledLums, g2) = shuffle GL.allLuminaries g1
      (shuffledDirs, g3) = shuffle (GL.allEnum :: [GL.Direction]) g2
  in  ( GL.emptyGameState numPlayers
                          (shuffledDirs Prelude.!! 0)
                          shuffledDeck
                          shuffledLums
      , g3
      )
