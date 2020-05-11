module Domain.User where

import           RIO

type UserId = Text

class Monad m => UserRepo m where
    createUserId :: m UserId
    isUserIdValid :: UserId -> m Bool

