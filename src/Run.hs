{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import qualified Data.Aeson
import qualified Data.Time                     as Time
import qualified RIO.Text.Lazy                 as TL
import qualified System.Random                 as Random
import qualified Web.Cookie                    as WC
import qualified Web.Scotty.Cookie             as SC
import           Web.Scotty.Trans

import           Import

import qualified Domain.Game.GameLogic         as GL
import qualified Domain.Game                   as D
import qualified Domain.User                   as DU

run :: RIO App ()
run = do
  logInfo "Starting application"
  port <- getOption optionsPort
  app  <- ask
  scottyT port (runRIO app) routes

type AppScotty a = ScottyT TL.Text (RIO App) a
type AppAction a = ActionT TL.Text (RIO App) a

userIdCookieName :: Text
userIdCookieName = "UserId"

getOrSetUserCookie :: AppAction DU.UserId
getOrSetUserCookie = do
  mayUid <- SC.getCookie userIdCookieName
  case mayUid of
    Just uid -> return uid
    Nothing  -> do
      uid                   <- lift DU.createUserId
      Time.UTCTime day time <- liftIO Time.getCurrentTime
      let expTime = Time.UTCTime (Time.addDays 2000 day) time
      let cookie = (SC.makeSimpleCookie userIdCookieName uid)
            { WC.setCookieExpires = Just expTime
            }
      SC.setCookie cookie
      return uid

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
  post "/creategame" $ do
    uid  <- getOrSetUserCookie
    resp <- lift $ D.createGame $ D.CreateGameRequest uid
    json resp
  post "/joingame/:gid" $ do
    uid  <- getOrSetUserCookie
    gid  <- param "gid"
    resp <- lift $ D.joinGame $ D.JoinGameRequest uid gid
    json resp
  post "/startgame/:gid" $ do
    uid  <- getOrSetUserCookie
    gid  <- param "gid"
    resp <- lift $ D.startGame $ D.StartGameRequest uid gid
    json resp
  post "/makemove/:gid" $ do
    uid               <- getOrSetUserCookie
    gid               <- param "gid"
    (move :: GL.Move) <- jsonData
    resp              <- lift $ D.makeMove $ D.MakeMoveRequest uid gid move
    json resp
  post "/viewgame/:gid" $ do
    uid  <- getOrSetUserCookie
    gid  <- param "gid"
    resp <- lift $ D.getGameView $ D.GetGameViewRequest uid gid
    json resp
  post "/listgames" $ do
    uid  <- getOrSetUserCookie
    resp <- lift $ D.getGamesForUser $ D.GetGamesForUserRequest uid
    json resp
  get "/sample" $ do
    gs <- createTestInitializedGameState 4
    json $ GL.computeViewForPlayer 0 gs
  get "/:name" $ do
    name <- param "name"
    text $ "hello " <> name
