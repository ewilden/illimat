{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
  ( run
  )
where

import qualified Data.Aeson
import qualified Data.Time                     as Time
import           Network.Wai.Middleware.Cors
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
userIdCookieName = "USER_ID"

setUserCookie :: DU.UserId -> AppAction ()
setUserCookie uid = do
  Time.UTCTime day time <- liftIO Time.getCurrentTime
  let expTime = Time.UTCTime (Time.addDays 7 day) time
  let cookie = (SC.makeSimpleCookie userIdCookieName uid)
        { WC.setCookieExpires = Just expTime
        }
  SC.setCookie cookie

getAndRefreshUserCookie :: AppAction DU.UserId
getAndRefreshUserCookie = do
  mayUid      <- SC.getCookie userIdCookieName
  mayValidUid <- case mayUid of
    Nothing  -> return Nothing
    Just uid -> do
      isValid <- lift $ DU.isUserIdValid uid
      if isValid then return $ Just uid else return Nothing
  case mayValidUid of
    Just uid -> do
      setUserCookie uid
      return uid
    Nothing -> do
      uid <- lift DU.createUserId
      setUserCookie uid
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
  -- middleware $ cors $ const $ Just $ simpleCorsResourcePolicy
  --   { corsOrigins        = Just (["http://localhost:3004"], True)
  --   , corsMethods        = ["GET", "POST"]
  --   , corsRequestHeaders = ["Authorization", "Content-Type"]
  --   }
  get "/gameapi/" $ text "hello"
  post "/gameapi/creategame" $ do
    uid  <- getAndRefreshUserCookie
    resp <- lift $ D.createGame $ D.CreateGameRequest uid
    json resp
  post "/gameapi/joingame/:gid" $ do
    uid  <- getAndRefreshUserCookie
    gid  <- param "gid"
    resp <- lift $ D.joinGame $ D.JoinGameRequest uid gid
    json resp
  post "/gameapi/startgame/:gid" $ do
    uid  <- getAndRefreshUserCookie
    gid  <- param "gid"
    resp <- lift $ D.startGame $ D.StartGameRequest uid gid
    json resp
  post "/gameapi/makemove/:gid" $ do
    uid               <- getAndRefreshUserCookie
    gid               <- param "gid"
    (move :: GL.Move) <- jsonData
    resp              <- lift $ D.makeMove $ D.MakeMoveRequest uid gid move
    json resp
  get "/gameapi/viewgame/:gid" $ do
    uid  <- getAndRefreshUserCookie
    gid  <- param "gid"
    resp <- lift $ D.getGameView $ D.GetGameViewRequest uid gid
    json resp
  get "/gameapi/listgames" $ do
    uid  <- getAndRefreshUserCookie
    resp <- lift $ D.getGamesForUser $ D.GetGamesForUserRequest uid
    json resp
  get "/gameapi/sample" $ do
    gs <- createTestInitializedGameState 4
    json $ GL.computeViewForPlayer 0 gs
  get "/gameapi/:name" $ do
    name <- param "name"
    text $ "hello " <> name
  -- options "/*" $ do

