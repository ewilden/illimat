module Adapter.InMemory.Game where

import           Control.Monad.Except
import           RIO
import           RIO.List
import qualified RIO.HashMap                   as HM
import qualified System.Random                 as Random
import           Text.StringRandom
import           Util

import           Domain.Game                   as D
import           Domain.Game.GameLogic         as GL

data State = State
    { _sGames :: !(HM.HashMap D.GameId D.Game)
    }

initialState :: State
initialState = State mempty

byUsers :: HM.HashMap D.GameId D.Game -> HM.HashMap D.UserId [D.Game]
byUsers = HM.foldr processGame HM.empty
 where
  processGame game partialMap =
    _gamePlayers game |> foldr (addEntry game) partialMap
  addEntry game player = HM.insertWith (<>) player [game]

class HasGameRelatedState env where
    gameRelatedStateL :: Lens' env (TVar State)

type InMemory r m = (HasGameRelatedState r, MonadReader r m, MonadIO m)

createGame :: InMemory r m => D.CreateGameRequest -> m D.CreateGameResponse
createGame req = do
  tvar <- view gameRelatedStateL
  gid  <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let hostId = D._cgreqGameOwner req
    let newGame = D.Game { D._gameId      = gid
                         , D._gameData    = D.GameDataStarting D.StartingGame
                         , D._gamePlayers = [hostId]
                         }
        newState = state { _sGames = HM.insert gid newGame $ _sGames state }
    writeTVar tvar newState
    return $ D.CreateGameResponse (D.computeViewForPlayer 0 newGame) gid

joinGame
  :: InMemory r m
  => D.JoinGameRequest
  -> m (Either D.JoinGameError D.JoinGameResponse)
joinGame (D.JoinGameRequest uid gid) = do
  tvar <- view gameRelatedStateL
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let mayGame :: Maybe D.Game
        mayGame = HM.lookup gid (_sGames state)
    case mayGame of
      Nothing   -> throwError D.JoinGameErrorNoSuchGame
      Just game -> do
        let usersInGame       = D._gamePlayers game
        let userAlreadyInGame = uid `elem` usersInGame
        let newPlayerIndex    = length usersInGame
        game
          |>  D._gameData
          >>> (\case
                D.GameDataRunning _ ->
                  throwError D.JoinGameErrorGameAlreadyRunning
                D.GameDataFinished _ ->
                  throwError D.JoinGameErrorGameAlreadyFinished
                D.GameDataStarting _ -> do
                  when userAlreadyInGame
                    $ throwError D.JoinGameErrorAlreadyInGame
                  -- add user to game
                  let
                    newGame = game { D._gamePlayers = usersInGame ++ [uid] }
                    newState =
                      state { _sGames = HM.insert gid newGame $ _sGames state }
                  lift $ writeTVar tvar newState
                  return $ D.JoinGameResponse $ D.computeViewForPlayer
                    newPlayerIndex
                    newGame
              )

startGame
  :: InMemory r m
  => D.StartGameRequest
  -> m (Either D.StartGameError D.StartGameResponse)
startGame (D.StartGameRequest uid gid) = do
  tvar    <- view gameRelatedStateL
  randgen <- liftIO Random.getStdGen
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let mayGame :: Maybe D.Game
        mayGame = HM.lookup gid (_sGames state)
    case mayGame of
      Nothing   -> throwError StartGameErrorNoSuchGame
      Just game -> do
        let usersInGame    = D._gamePlayers game
        let mayPlayerIndex = uid `elemIndex` usersInGame
        when (length usersInGame < 2)
          $ throwError StartGameErrorNotEnoughPlayers
        case (mayPlayerIndex, D._gameData game) of
          (Nothing, _                 ) -> throwError StartGameErrorNotInGame
          (Just 0 , GameDataStarting _) -> do
            let gsEmpty =
                  fst $ D.initEmptyGameState (length usersInGame) randgen
            initialGameState <-
              (case
                  mapLeft D.StartGameErrorOther
                  $   snd
                  <$> GL.runGS GL.doInitialGameSetup gsEmpty
                of
                  Left  err -> throwError err
                  Right gs  -> return gs
              )
            let newGame = game
                  { D._gameData = D.GameDataRunning $ D.RunningGame
                                    { D._rgGameState = initialGameState
                                    }
                  }
                newState =
                  state { _sGames = HM.insert gid newGame $ _sGames state }
            lift $ writeTVar tvar newState
            return $ D.StartGameResponse $ D.computeViewForPlayer 0 newGame
          (Just 0, GameDataRunning _) ->
            throwError StartGameErrorGameAlreadyStarted
          (Just 0, GameDataFinished _) ->
            throwError StartGameErrorGameAlreadyStarted
          (Just _, _) -> throwError StartGameErrorNotGameOwner

makeMove
  :: InMemory r m
  => D.MakeMoveRequest
  -> m (Either D.MakeMoveError D.MakeMoveResponse)
makeMove (D.MakeMoveRequest uid gid move) = do
  tvar <- view gameRelatedStateL
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let mayGame :: Maybe D.Game
        mayGame = HM.lookup gid (_sGames state)
    case mayGame of
      Nothing   -> throwError D.MakeMoveErrorNoSuchGame
      Just game -> do
        let usersInGame    = D._gamePlayers game
        let mayPlayerIndex = uid `elemIndex` usersInGame
        case mayPlayerIndex of
          Nothing          -> throwError D.MakeMoveErrorPlayerNotInGame
          Just playerIndex -> case D._gameData game of
            D.GameDataStarting _ -> throwError D.MakeMoveErrorGameNotStartedYet
            D.GameDataFinished _ ->
              throwError D.MakeMoveErrorGameAlreadyFinished
            D.GameDataRunning (D.RunningGame gameState) ->
              case flip GL.runGS gameState $ GL.executeMove playerIndex move of
                Left err -> throwError $ D.MakeMoveErrorInvalidMove err
                Right (_, nextGameState) -> do
                  let
                    newGame = game
                      { D._gameData = D.GameDataRunning
                                        (D.RunningGame nextGameState)
                      }
                    newState =
                      state { _sGames = HM.insert gid newGame $ _sGames state }
                  lift $ writeTVar tvar newState
                  return $ D.MakeMoveResponse $ D.computeViewForPlayer
                    playerIndex
                    newGame

getGameView
  :: InMemory r m
  => D.GetGameViewRequest
  -> m (Either D.GetGameViewError D.GetGameViewResponse)
getGameView (D.GetGameViewRequest uid gid) = do
  tvar  <- view gameRelatedStateL
  state <- liftIO $ readTVarIO tvar
  runExceptT $ case HM.lookup gid (_sGames state) of
    Nothing   -> throwError D.GetGameViewErrorNoSuchGame
    Just game -> case uid `elemIndex` (D._gamePlayers game) of
      Nothing -> throwError D.GetGameViewErrorUserNotInGame
      Just playerIndex ->
        return $ D.GetGameViewResponse $ D.computeViewForPlayer playerIndex game

getGamesForUser
  :: InMemory r m => D.GetGamesForUserRequest -> m D.GetGamesForUserResponse
getGamesForUser (D.GetGamesForUserRequest uid) = do
  tvar  <- view gameRelatedStateL
  state <- liftIO $ readTVarIO tvar
  return
    $ D.GetGamesForUserResponse
    $ map _gameId
    $ concat
    $ HM.lookup uid
    $ byUsers
    $ _sGames state
