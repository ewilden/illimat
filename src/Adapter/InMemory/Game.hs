{-# LANGUAGE LambdaCase #-}

module Adapter.InMemory.Game where

import ClassyPrelude
import           Control.Monad.Except
import           Data.Has
import           Text.StringRandom
import qualified GameLogic as GL
import qualified System.Random as Random
import qualified Prelude as Prelude ((!!))

import Control.Arrow

-- import qualified Domain.Auth as D.Auth
import qualified Domain.Game as D
-- import qualified GameLogic as GL

data State = State
  { _sGames :: Map D.GameId D.Game
  }

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

initialState :: State
initialState = State mempty

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

createGame :: InMemory r m => D.CreateGameRequest -> m (Either D.CreateGameError D.CreateGameResponse)
createGame req = do
  tvar <- asks getter
  gid <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let hostId = D._cgreqGameOwner req
        numPlayers = D._cgreqMaxNumPlayers req
    when (numPlayers < 2 || numPlayers > 4) $ throwError D.CreateGameErrorUnsupportedNumPlayers
    let newGame = D.Game 
                    { D._gameId = gid
                    , D._gameData = D.GameDataStarting $ D.StartingGame numPlayers
                    , D._gamePlayers = [hostId]
                    }
        newState = state
          { _sGames = insertMap gid newGame $ _sGames state 
          }
    lift $ writeTVar tvar newState
    return $ D.CreateGameResponse $ D.computeViewForPlayer 0 newGame
  
joinGame :: InMemory r m => D.JoinGameRequest -> m (Either D.JoinGameError D.JoinGameResponse)
joinGame (D.JoinGameRequest uid gid) = do
  randgen <- liftIO Random.getStdGen
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let mayGame :: Maybe D.Game
        mayGame = lookup gid (_sGames state)
    case mayGame of
      Nothing -> throwError D.JoinGameErrorNoSuchGame
      Just game -> do
        let usersInGame = D._gamePlayers game
        let userAlreadyInGame = any (uid ==) usersInGame
        let newPlayerIndex = length usersInGame
        game |> D._gameData >>> (\case 
          D.GameDataRunning _ -> throwError D.JoinGameErrorGameAlreadyRunning
          D.GameDataFinished _ -> throwError D.JoinGameErrorGameAlreadyFinished
          D.GameDataStarting (D.StartingGame maxPlayers) -> do
            when (maxPlayers == length usersInGame) $ throwError D.JoinGameErrorTooManyPlayers
            when userAlreadyInGame $ throwError D.JoinGameErrorAlreadyInGame

            -- add user to game
            let 
              gameWithPlayerAdded = game { 
                D._gamePlayers = usersInGame ++ [uid]
                }
              newGame = 
                -- if game full, switch to running game
                if 
                  (length . D._gamePlayers) gameWithPlayerAdded == maxPlayers
                then 
                  gameWithPlayerAdded {
                    D._gameData = D.GameDataRunning $ D.RunningGame {
                      D._rgGameState = fst $ initEmptyGameState maxPlayers randgen
                      }
                    }
                else gameWithPlayerAdded
              newState = state {
                _sGames = insertMap gid newGame $ _sGames state
                }
            lift $ writeTVar tvar newState
            return $ D.JoinGameResponse $ D.computeViewForPlayer newPlayerIndex newGame
          )
    
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
