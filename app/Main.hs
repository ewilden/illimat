module Main where

import ClassyPrelude hiding (delete)
import GameLogic
import ElmConnect
import qualified Prelude as Prelude ((!!))
import qualified System.Random as System.Random
import qualified Control.Monad.State.Lazy as LazyS
import Data.Aeson hiding (json)
import Data.Text.Conversions

import qualified Data.ByteString.Lazy as LBS (putStrLn)

import Web.Scotty.Trans
import Network.Wai.Middleware.Cors


-- from https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
shuffle :: [a] -> IO [a]
shuffle x = if length x < 2 then return x else do
    i <- System.Random.randomRIO (0, length(x)-1)
    r <- shuffle (take i x ++ drop (i+1) x)
    return (x Prelude.!! i : r)

initEmptyGameState :: Int -> IO GameState
initEmptyGameState numPlayers = do
  let shuffleCardsFor n
        | n >= 4 = shuffle allCards
        | otherwise = shuffle allCardsMinusStars
  shuffledDeck <- shuffleCardsFor numPlayers
  shuffledLums <- shuffle allLuminaries
  initSummerDir <- shuffle (allEnum :: [Direction]) <&> (Prelude.!! 0)
  return $ emptyGameState numPlayers initSummerDir shuffledDeck shuffledLums
  
blindlyDo :: GameState -> FailableGameAction a -> IO GameState
blindlyDo gs action = case LazyS.runStateT action gs of
  Left s -> error s
  Right (_, news) -> return news

-- main :: IO ()
-- main = do
--   gs <- initEmptyGameState 2
--   LBS.putStrLn $ encode $ gs
    
main :: IO ()
main = scottyT 3000 id $ do 
  middleware simpleCors
  routes

routes :: (MonadIO m) => ScottyT LText m ()
routes = do
  get "/start" $ do 
    gs <- liftIO $ initEmptyGameState 2
    gs' <- liftIO $ blindlyDo gs $ doInitialGameSetup
    json gs'