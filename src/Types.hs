{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

import qualified Adapter.InMemory.Game
import qualified Domain.Game

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsPort :: Int
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  , appGameRelatedState :: !(TVar Adapter.InMemory.Game.State)
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

getOption :: (MonadReader App m) => (Options -> a) -> m a
getOption getter = getter <$> asks appOptions

instance Adapter.InMemory.Game.HasGameRelatedState App where
  gameRelatedStateL = lens appGameRelatedState (\x y -> x {appGameRelatedState = y})

instance Domain.Game.GameRepo (RIO App) where
  createGame = Adapter.InMemory.Game.createGame
  joinGame = Adapter.InMemory.Game.joinGame
  startGame = Adapter.InMemory.Game.startGame
  makeMove = Adapter.InMemory.Game.makeMove
  getGameView = Adapter.InMemory.Game.getGameView
  getGamesForUser = Adapter.InMemory.Game.getGamesForUser
