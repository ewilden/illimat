module Domain.Auth
  (
  -- * Types
    Auth(..)
  , Username
  , mkUsername
  , rawUsername
  , Password
  , mkPassword
  , rawPassword
  , UserId
  , SessionId
  , RegistrationError(..)
  , LoginError(..)
  , UsernameVerificationError(..)
  , VerificationCode
  ,

  -- * Ports
    AuthRepo(..)
  , UsernameVerificationNotif(..)
  , SessionRepo(..)
  ,

  -- * Use cases
    register
  , verifyUsername
  , login
  , resolveSessionId
  , getUser
  )
where

import           ClassyPrelude
import           Control.Monad.Except
import Katip

type VerificationCode = Text

data RegistrationError
  = RegistrationErrorUsernameTaken
  deriving (Show, Eq)

data UsernameVerificationError
  = UsernameVerificationErrorInvalidCode
  deriving (Show, Eq)

newtype Username = Username { usernameRaw :: Text } deriving (Show, Eq, Ord)

rawUsername :: Username -> Text
rawUsername = usernameRaw
mkUsername :: Text -> Either [Text] Username
mkUsername = capLengthAt 50 Username

capLengthAt :: Int -> (Text -> a) -> Text -> Either [Text] a
capLengthAt maxLength ctor raw = if length raw <= 50
  then Right $ ctor raw
  else Left ["\"" ++ raw ++ "\" is too long to be valid"]

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)
rawPassword :: Password -> Text
rawPassword = passwordRaw
mkPassword :: Text -> Either [Text] Password
mkPassword = capLengthAt 50 Password

data Auth = Auth
 { authUsername :: Username
 , authPassword :: Password
 } deriving (Show, Eq)

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  findUserByAuth :: Auth -> m (Maybe UserId)
  findUsernameFromUserId :: UserId -> m (Maybe Username)
  setUsernameAsVerified :: VerificationCode -> m (Either UsernameVerificationError (UserId, Username))

getUser :: AuthRepo m => UserId -> m (Maybe Username)
getUser = findUsernameFromUserId

verifyUsername
  :: AuthRepo m => VerificationCode -> m (Either UsernameVerificationError ())
verifyUsername = ((const () <$>) <$>) . setUsernameAsVerified

class Monad m => UsernameVerificationNotif m where
  notifyUsernameVerification :: Username -> VerificationCode -> m ()

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register
  :: (KatipContext m, AuthRepo m, UsernameVerificationNotif m)
  => Auth
  -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uid, vCode) <- ExceptT $ addAuth auth
  let username = authUsername auth
  lift $ notifyUsernameVerification username vCode
  withUserIdContext uid $
    $(logTM) InfoS $ ls (rawUsername username) <> " is registered successfully"

type UserId = Int
type SessionId = Text

data LoginError = LoginErrorInvalidAuth
  deriving (Show, Eq)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing  -> throwError LoginErrorInvalidAuth
    Just uId -> withUserIdContext uId . lift $ do
      sId <- newSession uId
      $(logTM) InfoS $ ls (rawUsername $ authUsername auth) <> " logged in successfully"
      return sId

