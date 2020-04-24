module Lib where

import ClassyPrelude
import Control.Monad.Catch hiding (bracket)
import Control.Monad.Fail
import Katip

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.InMemory.Game as G
import qualified Adapter.PostgreSQL.Auth as PG
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

type State =  (PG.State, TVar M.State)
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, KatipContext, Katip, Monad, MonadReader State, MonadIO, MonadFail, MonadThrow)

run :: LogEnv -> State -> App a -> IO a
run le state 
  = runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp

instance AuthRepo App where
  addAuth = PG.addAuth
  setUsernameAsVerified = PG.setUsernameAsVerified
  findUserByAuth = ((fst <$>) <$>) . PG.findUserByAuth
  findUsernameFromUserId = PG.findUsernameFromUserId

instance UsernameVerificationNotif App where
  notifyUsernameVerification = M.notifyUsernameVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

someFunc :: IO ()
someFunc = withKatip $ \le -> do
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState -> run le (pgState, mState) action
  where
    pgCfg = PG.Config
            { PG.configUrl = "postgresql://localhost/hauth" 
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe = 5
            , PG.configIdleConnTimeout = 10
            }

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

runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no namespace"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Log in ns1"
  katipAddNamespace "ns2" $ do
    $(logTM) WarningS "Log in ns2"
    katipAddNamespace "ns3" $
      katipAddContext (sl "userId" $ asText "12") $ do
        $(logTM) InfoS "Log in ns2.ns3 with userId context"
        katipAddContext (sl "country" $ asText "Singapore") $
         $(logTM) InfoS "Log in ns2.ns3 with userId and country context"
