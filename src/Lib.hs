module Lib where

import ClassyPrelude
import Control.Monad.Fail

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.InMemory.Game as G
import Domain.Auth
import Domain.Game

type GameRepoState = TVar G.State
newtype GameStack a = GameStack
  { unGameStack :: ReaderT GameRepoState IO a
  } deriving (Applicative, Functor, Monad, MonadReader GameRepoState, MonadIO, MonadFail)

runGameStack :: GameRepoState -> GameStack a -> IO a
runGameStack state = flip runReaderT state . unGameStack

instance GameRepo GameStack where
  createGame = G.createGame
  joinGame = G.joinGame
  makeMove = G.makeMove
  getGameView = G.getGameView
  getGamesForUser = G.getGamesForUser

type State = TVar M.State
newtype App a = App
  { unApp :: ReaderT State IO a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadFail)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance AuthRepo App where
  addAuth = M.addAuth
  setUsernameAsVerified = M.setUsernameAsVerified
  findUserByAuth = ((fst <$>) <$>) . M.findUserByAuth
  findUsernameFromUserId = M.findUsernameFromUserId

instance UsernameVerificationNotif App where
  notifyUsernameVerification = M.notifyUsernameVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

someFunc :: IO ()
someFunc = do
  state <- newTVarIO M.initialState
  run state action

action :: App ()
action = do
  let username = either (error "impossible") id $ mkUsername "usern"
      passw = either (error "impossible") id $ mkPassword "1234ABCDefgh"
      auth = Auth username passw
  _ <- register auth
  Just vCode <- M.getNotificationsForUsername username
  _ <- verifyUsername vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredUsername <- getUser uId
  print (session, uId, registeredUsername)

-- left off file page 83
