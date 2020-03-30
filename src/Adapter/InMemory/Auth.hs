module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Data.Has

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
  :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = undefined

setUsernameAsVerified
  :: InMemory r m
  => D.VerificationCode
  -> m (Either D.UsernameVerificationError ())
setUsernameAsVerified = undefined

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

findUsernameFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Username)
findUsernameFromUserId = undefined

notifyUsernameVerification
  :: InMemory r m => D.Username -> D.VerificationCode -> m ()
notifyUsernameVerification = undefined

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession = undefined

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId = undefined
