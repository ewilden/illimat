module Adapter.PostgreSQL.Auth where

import ClassyPrelude hiding (try, bracket)
import Control.Monad.Catch
import Data.Pool
import Data.Time
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple

import qualified Domain.Auth as D
import Data.Has
import Text.StringRandom

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

addAuth :: PG r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth username pass) = do
  let rawUsername = D.rawUsername username
      rawPassw = D.rawPassword pass
  -- generate vCode
  vCode <- liftIO $ do
    r <- stringRandomIO "[A-Za-z0-9]{16}"
    return $ (tshow rawUsername) <> "_" <> r
  -- issue query
  result <- withConn $ \conn ->
    try $ query conn qry (rawUsername, rawPassw, vCode)
  -- interpret result
  case result of
    Right [Only uId] -> return $ Right (uId, vCode)
    Right _ -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
      if state == "23505" && "auths_username_key" `isInfixOf` msg
        then return $ Left D.RegistrationErrorUsernameTaken
        else throwString $ "Unhandled PG exception: " <> show err
  where
    qry = "insert into auths \
          \(username, pass, username_verification_code, is_username_verified) \
          \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

setUsernameAsVerified :: PG r m
  => D.VerificationCode
  -> m (Either D.UsernameVerificationError (D.UserId, D.Username))
setUsernameAsVerified vCode = do
  result <- withConn $ \conn -> query conn qry (Only vCode)
  case result of
    [(uId, uname)] -> case D.mkUsername uname of
      Right username -> return $ Right (uId, username)
      _ -> throwString $ "Should not happen: username in DB is not valid: " <> unpack uname
    _ -> return $ Left D.UsernameVerificationErrorInvalidCode
  where
    qry = "update auths \
          \set is_username_verified = 't' \
          \where username_verification_code = ? \
          \returning id, cast (username as text)"

findUserByAuth :: PG r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth username pass) = do
  let rawUsername = D.rawUsername username
      rawPassw = D.rawPassword pass
  result <- withConn $ \conn -> query conn qry (rawUsername, rawPassw)
  return $ case result of
    [(uId, isVerified)] -> Just (uId, isVerified)
    _ -> Nothing
  where
    qry = "select id, is_username_verified \
          \from auths \
          \where username = ? and pass = crypt(?, pass)"

findUsernameFromUserId :: PG r m => D.UserId -> m (Maybe D.Username)
findUsernameFromUserId uId = do
  result <- withConn $ \conn -> query conn qry (Only uId)
  case result of
      [Only uname] -> case D.mkUsername uname of
        Right username -> return $ Just username
        _ -> throwString $ "Should not happen: username in DB is not valid: " <> unpack uname
      _ -> return Nothing
  where
    qry = "select cast(username as text) \
          \from auths \
          \where id = ?"

type State = Pool Connection

data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout :: NominalDiffTime
  }

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> return ()
  where
    cmds = [ MigrationInitialization
      , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
      ]

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg action =
  bracket initPool cleanPool action
  where
    initPool = createPool openConn closeConn
      (configStripeCount cfg)
      (configIdleConnTimeout cfg)
      (configMaxOpenConnPerStripe cfg)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action state
