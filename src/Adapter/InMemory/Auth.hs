module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Control.Monad.Except
import           Data.Has
import           Text.StringRandom

import qualified Domain.Auth                   as D

data State = State
 { stateAuths :: [(D.UserId, D.Auth)]
 , stateUnverifiedUsernames :: Map D.VerificationCode D.Username
 , stateVerifiedUsernames :: Set D.Username
 , stateUserIdCounter :: Int
 , stateNotifications :: Map D.Username D.VerificationCode
 , stateSessions :: Map D.SessionId D.UserId
 } deriving (Show, Eq)

initialState :: State
initialState = State { stateAuths               = []
                     , stateUnverifiedUsernames = mempty
                     , stateVerifiedUsernames   = mempty
                     , stateUserIdCounter       = 0
                     , stateNotifications       = mempty
                     , stateSessions            = mempty
                     }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth
  :: InMemory r m => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter

  -- gen verification code
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"

  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- does this username already exist?
    let auths = stateAuths state
        username = D.authUsername auth
        isDuplicate = any (username ==) . map (D.authUsername . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorUsernameTaken
    -- update the state
    let newUserId = stateUserIdCounter state + 1
        newAuths = (newUserId, auth) : auths
        unverifieds = stateUnverifiedUsernames state
        newUnverifieds = insertMap vCode username unverifieds
        newState = state
          { stateAuths = newAuths
          , stateUserIdCounter = newUserId
          , stateUnverifiedUsernames = newUnverifieds 
          }
    lift $ writeTVar tvar newState
    return (newUserId, vCode)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = return a

setUsernameAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.UsernameVerificationError (D.UserId, D.Username))
setUsernameAsVerified vcode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let mayUsername = (vcode `lookup`) $ stateUnverifiedUsernames state
    case mayUsername of
      Nothing       -> throwError D.UsernameVerificationErrorInvalidCode
      Just username -> do
        let newUnverifieds =
              (vcode `deleteMap`) $ stateUnverifiedUsernames state
            newVerifieds =
              (username `insertSet`) $ stateVerifiedUsernames state
            newState = state { stateUnverifiedUsernames = newUnverifieds
                             , stateVerifiedUsernames   = newVerifieds
                             }
            mayUid = map fst . find ((username ==) . D.authUsername . snd) $ stateAuths $ newState
        uid <- mayUid `orThrow` D.UsernameVerificationErrorInvalidCode
        lift $ writeTVar tvar newState
        return (uid, username)

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let isVerified = (D.authUsername auth `elem`) $ stateVerifiedUsernames state
      mayUid     = map fst . find ((auth ==) . snd) $ stateAuths state
  return $ (, isVerified) <$> mayUid

findUsernameFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Username)
findUsernameFromUserId uId = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authUsername <$> mayAuth

notifyUsernameVerification
  :: InMemory r m => D.Username -> D.VerificationCode -> m ()
notifyUsernameVerification username vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications    = stateNotifications state
        newNotifications = insertMap username vCode notifications
        newState         = state { stateNotifications = newNotifications }
    writeTVar tvar newState

getNotificationsForUsername
  :: InMemory r m => D.Username -> m (Maybe D.VerificationCode)
getNotificationsForUsername username = do
  tvar  <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup username $ stateNotifications state

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId  <- liftIO $ ((tshow uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions    = stateSessions state
        newSessions = insertMap sId uId sessions
        newState    = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

