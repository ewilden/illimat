module Adapter.InMemory.User where

import           Crypto.TripleSec
import qualified Data.ByteString.Base64.URL    as Base64
import           RIO
import           RIO.Text
import           Text.StringRandom

import qualified Domain.User                   as D

type UserIdCipherKey = ByteString
type UserId = D.UserId

class HasUserIdCipherKey env where
    userIdCipherKeyL :: Lens' env UserIdCipherKey

type InMemory r m = (HasUserIdCipherKey r, MonadReader r m, MonadIO m)

userIdPrefix :: Text
userIdPrefix = "userIdPrefix"

encryptToBase64 :: (InMemory r m) => Text -> m ByteString
encryptToBase64 plaintext = do
    pass       <- view userIdCipherKeyL
    ciphertext <- liftIO $ encryptIO pass $ encodeUtf8 plaintext
    return $ Base64.encode ciphertext

decryptFromBase64 :: (InMemory r m) => ByteString -> m Text
decryptFromBase64 base64ciphertext = do
    let ciphertext = Base64.decodeLenient base64ciphertext
    pass          <- view userIdCipherKeyL
    utf8plaintext <- liftIO $ decryptIO pass $ ciphertext
    return $ decodeUtf8Lenient utf8plaintext

createUserId :: (InMemory r m) => m UserId
createUserId = do
    randId <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
    let plaintext = userIdPrefix <> randId
    encryptToBase64 plaintext

isUserIdValid :: (InMemory r m) => UserId -> m Bool
isUserIdValid userId = do
    plaintext <- decryptFromBase64 userId
    return $ userIdPrefix `isPrefixOf` plaintext
