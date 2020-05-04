{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import qualified RIO.Text.Lazy                 as TL
import qualified System.Random                 as Random
import           Web.Scotty.Trans

import           Import

import qualified Domain.Game.GameLogic         as GL
import qualified Domain.Game                   as D

run :: RIO App ()
run = do
  logInfo "Starting application"
  port <- getOption optionsPort
  app  <- ask
  scottyT port (runRIO app) routes

createTestInitializedGameState :: (MonadIO m) => Int -> m GL.GameState
createTestInitializedGameState numPlayers = do
  randgen <- liftIO Random.newStdGen
  case
      GL.runGS GL.doInitialGameSetup $ fst $ D.initEmptyGameState numPlayers
                                                                  randgen
    of
      Left  _       -> error "impossible"
      Right (_, gs) -> return gs

routes :: ScottyT TL.Text (RIO App) ()
routes = do
  get "/" $ text "hello"
  get "/sample" $ do
    gs <- createTestInitializedGameState 4
    json $ GL.computeViewForPlayer 0 gs
  get "/:name" $ do
    name <- param "name"
    text $ "hello " <> name
