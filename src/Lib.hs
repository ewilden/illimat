module Lib
    ( someFunc
    ) where

import ClassyPrelude hiding (pred, succ)
import qualified ClassyPrelude as Unsafe (pred, succ)
import Control.Monad.State.Lazy

succ :: (Bounded a, Eq a, Enum a) => a -> a
succ a = if a == maxBound then minBound else Unsafe.succ a

pred :: (Bounded a, Eq a, Enum a) => a -> a
pred a = if a == minBound then maxBound else Unsafe.pred a

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GameState = GameState 
    { gameIllimatState :: IllimatState
    , gameBoardState :: BoardState
    , gamePlayerState :: [PlayerState]
    , gameDeck :: Deck
    } deriving (Show)

data PlayerState = PlayerState
    { playerHand :: [Card]
    , playerNumOkuses :: Int
    , playerLuminaries :: [Luminary]
    , playerHarvestPile :: [Card]
    } deriving (Show)

data IllimatState = IllimatState
    { illSummerDir :: Direction
    , illNumOkuses :: Int
    } deriving (Show)

data BoardState = BoardState
    { bsFieldN :: FieldState
    , bsFieldE :: FieldState
    , bsFieldS :: FieldState
    , bsFieldW :: FieldState
    } deriving (Show)

data FieldState = FieldState 
    { fieldCards :: [CardStack]
    , fieldLuminary :: LuminaryState
    } deriving (Show)

data LuminaryState = FaceUp Luminary | FaceDown Luminary | NoLuminary deriving (Show)
data Luminary = Union | Maiden | Rake | River | Changeling | Newborn | Forest_Queen | Children
    deriving (Show, Eq, Enum, Bounded)

data CardStack = CardStack CardVal [Card] deriving Show
data CardVal = Fool | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Knight | Queen | King 
    deriving (Show, Eq, Enum, Bounded)
data Card = Card CardVal CardSeason deriving Show
data CardSeason = CSummer | CSpring | CWinter | CAutumn 
    deriving (Show, Eq, Enum, Bounded)

data Direction = N | E | S | W 
    deriving (Show, Eq, Enum, Bounded)

enumStartingFrom :: (Enum a, Bounded a, Eq a) => a -> [a]
enumStartingFrom startVal = 
    if startVal == maxBound 
    then [startVal] 
    else startVal : enumStartingFrom (succ startVal)

allEnum :: (Enum a, Bounded a, Eq a) => [a]
allEnum = enumStartingFrom minBound

allCards :: [Card]
allCards = Card <$> (allEnum :: [CardVal]) <*> (allEnum :: [CardSeason])

type Deck = [Card]
type LuminaryDeck = [Luminary]

emptyGameState :: Int -> Direction -> Deck -> GameState
emptyGameState numPlayers initSummerDir startingDeck =
    GameState
    { gameIllimatState = IllimatState initSummerDir 4
    , gameBoardState = emptyBoard
    , gamePlayerState = replicate numPlayers emptyPlayer
    , gameDeck = startingDeck
    }
    where
        emptyBoard = BoardState emptyField emptyField emptyField emptyField
        emptyPlayer = PlayerState [] 0 [] []
        emptyField = FieldState [] NoLuminary

type PlayerIndex = Int
type GameStateT = State GameState

patchListAt :: Int -> (a -> a) -> [a] -> [a]
patchListAt n f = unpairIndex . (mapIfIndexIs n f) . pairIndex
    where 
        pairIndex = pairIndexAux 0
        pairIndexAux n (h:tl) = (h,n) : pairIndexAux (n+1) tl
        pairIndexAux _ [] = []
        unpairIndex = map fst
        mapIfIndexIs n f = map (\(a, i) -> if i == n then (f a, i) else (a, i))

dealCardToPlayer :: PlayerIndex -> GameState -> ((), GameState)
dealCardToPlayer playerIndex (GameState ill board players (topCard:restOfDeck)) =
    (
        (),
        GameState
        { gameIllimatState = ill
        , gameBoardState = board
        , gamePlayerState = patchListAt playerIndex (mapHand (topCard :)) players
        , gameDeck = restOfDeck
        }
    )

mapHand :: ([Card] -> [Card]) -> PlayerState -> PlayerState
mapHand f (PlayerState hand okuses luminaries pile) = 
    PlayerState (f hand) okuses luminaries pile
        