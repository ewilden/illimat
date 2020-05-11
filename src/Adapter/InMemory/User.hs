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

type InMemory r m
    = (HasUserIdCipherKey r, MonadReader r m, MonadIO m, MonadUnliftIO m)

userIdPrefix :: Text
userIdPrefix = "userIdPrefix"

encryptToBase64 :: (InMemory r m) => Text -> m Text
encryptToBase64 plaintext = do
    pass <- view userIdCipherKeyL
    plaintext
        &   encodeUtf8
        &   encryptIO pass
        &   liftIO
        <&> Base64.encode
        <&> decodeUtf8Lenient

decryptFromBase64 :: (InMemory r m) => Text -> m Text
decryptFromBase64 base64ciphertext = do
    pass <- view userIdCipherKeyL
    base64ciphertext
        &   encodeUtf8
        &   Base64.decodeLenient
        &   decryptIO pass
        &   liftIO
        <&> decodeUtf8Lenient

createUserId :: (InMemory r m) => m UserId
createUserId = do
    randId <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
    let plaintext = userIdPrefix <> randId
    encryptToBase64 plaintext

isUserIdValid :: (InMemory r m) => UserId -> m Bool
isUserIdValid userId = do
    eithPlaintext <- try $ decryptFromBase64 userId
    case eithPlaintext of
        Left (_ :: TripleSecException) -> return False
        Right plaintext -> return $ userIdPrefix `isPrefixOf` plaintext
