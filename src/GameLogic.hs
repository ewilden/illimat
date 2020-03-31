{-# LANGUAGE TemplateHaskell #-}
module GameLogic where

import           Prelude                        ( head )
import           ClassyPrelude           hiding ( pred
                                                , succ
                                                , sequence_
                                                )
import qualified ClassyPrelude                 as Unsafe
                                                ( pred
                                                , succ
                                                )
import           Control.Monad.State.Lazy
import           Data.Sort

import           Data.Text.Conversions

import           Elm.Derive
import           Elm.Module

-- import           Data.Aeson
import           Data.Proxy

import           Control.Lens
import           Control.Lens.TH

import           ElmConnect                     ( myOptions )

-- loops around back to minBound
succ :: (Bounded a, Eq a, Enum a) => a -> a
succ a = if a == maxBound then minBound else Unsafe.succ a

pred :: (Bounded a, Eq a, Enum a) => a -> a
pred a = if a == minBound then maxBound else Unsafe.pred a

headOr :: a -> [a] -> a
headOr _ (h : tl) = h
headOr a []       = a

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

(!!) :: [a] -> Int -> Maybe a
[]       !! _ = Nothing
(h : tl) !! 0 = Just h
(h : tl) !! n | n < 0     = Nothing
              | otherwise = tl !! (n - 1)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GameState = GameState
    { _gameIllimatState :: IllimatState
    , _gameBoardState :: BoardState
    , _gamePlayerState :: [PlayerState]
    , _gameDeck :: [Card]
    , _gameUnusedLuminaries :: [Luminary]
    , _gameChildrenCards :: [Card]
    } deriving (Show)

boxIdent :: (a -> a) -> (a -> Identity a)
boxIdent f = Identity . f

unboxIdent :: (a -> Identity a) -> (a -> a)
unboxIdent f = (\(Identity a) -> a) . f

mapIllimat :: (IllimatState -> IllimatState) -> GameState -> GameState
mapIllimat f gs = gs { _gameIllimatState = f $ _gameIllimatState gs }

mapIllimatM
  :: (Monad m) => (IllimatState -> m IllimatState) -> GameState -> m GameState
mapIllimatM f gs =
  (\is -> gs { _gameIllimatState = is }) <$> (f $ _gameIllimatState gs)

mapBoard :: (BoardState -> BoardState) -> GameState -> GameState
mapBoard = unboxIdent . mapBoardM . boxIdent

mapBoardM
  :: (Monad m) => (BoardState -> m BoardState) -> GameState -> m GameState
mapBoardM f gs =
  (\bs -> gs { _gameBoardState = bs }) <$> (f $ _gameBoardState gs)

mapField :: Direction -> (FieldState -> FieldState) -> GameState -> GameState
mapField dir f = unboxIdent (mapFieldM dir (boxIdent f))

mapFieldM
  :: (Monad m)
  => Direction
  -> (FieldState -> m FieldState)
  -> GameState
  -> m GameState
mapFieldM dir f gs = do
  let targetedField = fieldFromDir dir gs
  nextField <- f targetedField
  return $ fieldPutter dir gs nextField

mapPlayerState :: ([PlayerState] -> [PlayerState]) -> GameState -> GameState
mapPlayerState f gs = gs { _gamePlayerState = f $ _gamePlayerState gs }

data PlayerState = PlayerState
    { _playerHand :: [Card]
    , _playerNumOkuses :: Int
    , _playerLuminaries :: [Luminary]
    , _playerHarvestPile :: [Card]
    } deriving (Show)

data IllimatState = IllimatState
    { _illSummerDir :: Direction
    , _illNumOkuses :: Int
    } deriving (Show)

data BoardState = BoardState
    { _bsFieldN :: FieldState
    , _bsFieldE :: FieldState
    , _bsFieldS :: FieldState
    , _bsFieldW :: FieldState
    } deriving (Show)

data FieldState = FieldState
    { _fieldCards :: [CardStack]
    , _fieldLuminary :: LuminaryState
    } deriving (Show)

data LuminaryState = FaceUp Luminary | FaceDown Luminary | NoLuminary deriving (Show, Eq)
data Luminary = Union | Maiden | Rake | River | Changeling | Newborn | Forest_Queen | Children
    deriving (Show, Eq, Enum, Bounded)

data CardStack = CardStack [Int] [Card] deriving (Show, Eq)
data CardVal = Fool | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Knight | Queen | King
    deriving (Show, Eq, Enum, Bounded)
data Card = Card CardVal CardSeason deriving (Show, Eq)
data CardSeason = CSummer | CSpring | CWinter | CAutumn | CStars
    deriving (Show, Eq, Enum, Bounded)

data Season = Summer | Spring | Winter | Autumn
    deriving (Show, Eq, Enum, Bounded)

data Direction = N | E | S | W
    deriving (Show, Eq, Enum, Bounded)

fieldSFromDir :: Direction -> GameState -> (FieldState, Season)
fieldSFromDir dir gs =
  (fieldGetter dir $ (_gameBoardState gs), seasonFromDir dir gs)

fieldGetter :: Direction -> BoardState -> FieldState
fieldGetter N = _bsFieldN
fieldGetter S = _bsFieldS
fieldGetter E = _bsFieldE
fieldGetter W = _bsFieldW

fieldPutter :: Direction -> GameState -> (FieldState -> GameState)
fieldPutter N gs fs = mapBoard (\bs -> bs { _bsFieldN = fs }) gs
fieldPutter S gs fs = mapBoard (\bs -> bs { _bsFieldS = fs }) gs
fieldPutter E gs fs = mapBoard (\bs -> bs { _bsFieldE = fs }) gs
fieldPutter W gs fs = mapBoard (\bs -> bs { _bsFieldW = fs }) gs

fieldFromDir :: Direction -> GameState -> FieldState
fieldFromDir dir = fst . (fieldSFromDir dir)

getFieldS :: Direction -> FailableGameAction FieldState
getFieldS dir = fieldFromDir dir <$> get

seasonFromDir :: Direction -> GameState -> Season
seasonFromDir dir gs | dir == (_illSummerDir $ _gameIllimatState gs) = Summer
                     | otherwise = pred $ seasonFromDir (succ dir) gs

enumStartingFrom :: (Enum a, Bounded a, Eq a) => a -> [a]
enumStartingFrom startVal = if startVal == maxBound
  then [startVal]
  else startVal : enumStartingFrom (succ startVal)

allEnum :: (Enum a, Bounded a, Eq a) => [a]
allEnum = enumStartingFrom minBound

allCards :: [Card]
allCards = Card <$> (allEnum :: [CardVal]) <*> (allEnum :: [CardSeason])

allCardsMinusStars :: [Card]
allCardsMinusStars = filter notStar allCards
 where
  notStar (Card _ CStars) = False
  notStar _               = True

allLuminaries :: [Luminary]
allLuminaries = allEnum

emptyGameState :: Int -> Direction -> [Card] -> [Luminary] -> GameState
emptyGameState numPlayers initSummerDir startingDeck startingLuminaryDeck =
  GameState { _gameIllimatState     = IllimatState initSummerDir numPlayers
            , _gameBoardState       = emptyBoard
            , _gamePlayerState      = replicate numPlayers emptyPlayer
            , _gameDeck             = startingDeck
            , _gameUnusedLuminaries = startingLuminaryDeck
            , _gameChildrenCards    = []
            }
 where
  emptyBoard  = BoardState emptyField emptyField emptyField emptyField
  emptyPlayer = PlayerState [] 0 [] []
  emptyField  = FieldState [] NoLuminary

twoPlayerDefaultState :: GameState
twoPlayerDefaultState = emptyGameState 2 N allCardsMinusStars allLuminaries

type PlayerIndex = Int
type GameStateM = State GameState

patchListAt :: Int -> (a -> a) -> [a] -> Maybe [a]
patchListAt n f l = case splitAt n l of
  (pre, x : post) -> Just $ pre ++ ((f x) : post)
  _               -> Nothing

-- <&> = flip <$>

patchListEith :: s -> Int -> (a -> Either s a) -> [a] -> Either s [a]
patchListEith failMsg ind f ls = case splitAt ind ls of
  (pre, x : post) -> (f x) <&> (: post) <&> (pre ++)
  _               -> Left failMsg

type FailableGameAction a = StateT GameState (Either String) a

runGS :: FailableGameAction a -> GameState -> Either String (a, GameState)
runGS action gs = (runStateT action gs)

toEith :: s -> Maybe a -> Either s a
toEith _ (Just a) = Right a
toEith s Nothing  = Left s

dealCardToPlayer :: PlayerIndex -> FailableGameAction ()
dealCardToPlayer playerIndex = do
  currDeck <- withS _gameDeck
  case currDeck of
    []       -> returnLeft "No more cards!"
    (h : tl) -> do
      updateState (\gs -> return $ gs { _gameDeck = tl })
      updateState $ mapPlayerM
        playerIndex
        (\ps -> return $ ps { _playerHand = h : (_playerHand ps) })

dealUntilFieldHasNCards :: Int -> Direction -> FailableGameAction ()
dealUntilFieldHasNCards n fieldDir = do
  currGS <- get
  let currDeck    = _gameDeck currGS
      currField   = fieldFromDir fieldDir currGS
      cardsToDeal = max 0 (n - (length $ _fieldCards currField))
  if (length currDeck) >= cardsToDeal
    then do
      updateState
        $ (\gs -> return $ gs { _gameDeck = drop cardsToDeal currDeck })
      updateState $ mapFieldM
        fieldDir
        (\fs -> return $ fs
          { _fieldCards = (map fromCard $ take cardsToDeal currDeck)
                            ++ _fieldCards currField
          }
        )
    else returnLeft "Didn't have enough cards to re-seed the field!"

deal3CardsToField :: Direction -> FailableGameAction ()
deal3CardsToField fieldDir = dealUntilFieldHasNCards 3 fieldDir

dealLuminaryToField :: Direction -> FailableGameAction ()
dealLuminaryToField dir = do
  unusedLums <- withS _gameUnusedLuminaries
  case unusedLums of
    []       -> returnLeft "No remaining luminaries"
    (h : tl) -> do
      updateState (\gs -> return $ gs { _gameUnusedLuminaries = tl })
      updateState
        $ mapFieldM dir (\fs -> return $ fs { _fieldLuminary = FaceDown h })

doAllowingFailure :: FailableGameAction () -> FailableGameAction Bool
doAllowingFailure action = do
  succeeded <- try' action
  case succeeded of
    True  -> action >> (return True)
    False -> return False

mapHand :: ([Card] -> [Card]) -> PlayerState -> PlayerState
mapHand f (PlayerState hand okuses luminaries pile) =
  PlayerState (f hand) okuses luminaries pile

getPlayer :: PlayerIndex -> GameState -> Maybe PlayerState
getPlayer ind gs = ((!! ind) . _gamePlayerState) gs

getPlayerS :: PlayerIndex -> FailableGameAction PlayerState
getPlayerS ind = do
  currGS <- get
  case getPlayer ind currGS of
    Nothing     -> returnLeft "That player doesn't exist!"
    Just player -> return player

returnLeft :: String -> FailableGameAction a
returnLeft s = StateT (\_ -> Left s)

removeCardStackFromField :: CardStack -> Direction -> FailableGameAction ()
removeCardStackFromField cardStack dir = do
  currGS <- get
  let field = fieldFromDir dir currGS
  case cardStack `elem` (_fieldCards field) of
    True -> do
      put $ mapField
        dir
        (\(FieldState cards luminary) ->
          FieldState (filter (/= cardStack) cards) luminary
        )
        currGS
      return ()
    False -> returnLeft "Card stack not in field!"

wouldSucceed :: GameState -> FailableGameAction a -> Bool
wouldSucceed s action = case (runStateT action s) of
  Right _ -> True
  Left  _ -> False

try' :: FailableGameAction a -> FailableGameAction Bool
try' action = do
  s <- get
  return $ wouldSucceed s action

tryAll :: [FailableGameAction a] -> FailableGameAction Bool
tryAll actions = (all id) <$> (sequence (map try' actions))

fromCard :: Card -> CardStack
fromCard card@(Card val season) = CardStack (toNumberVals val) [card]

removeCardFromField :: Card -> Direction -> FailableGameAction ()
removeCardFromField = removeCardStackFromField . fromCard

withS :: (GameState -> a) -> FailableGameAction a
withS f = f <$> get

updateStatePure :: (GameState -> GameState) -> FailableGameAction ()
updateStatePure f = updateState (return . f)

updateState :: (GameState -> Either String GameState) -> FailableGameAction ()
updateState f = do
  s <- get
  case f s of
    Left  errMsg -> returnLeft errMsg
    Right gs     -> put gs

mapPlayerM
  :: PlayerIndex
  -> (PlayerState -> Either String PlayerState)
  -> (GameState -> Either String GameState)
mapPlayerM playerInd f gs =
  let prevPlayers = _gamePlayerState gs
      patchResult = patchListEith "no such player" playerInd f prevPlayers
  in  (\nextPlayers -> gs { _gamePlayerState = nextPlayers }) <$> patchResult

checkS :: Bool -> String -> FailableGameAction ()
checkS cond failureMsg = if cond then return () else returnLeft failureMsg

checkNotS :: Bool -> String -> FailableGameAction ()
checkNotS failCond failureMsg =
  if failCond then returnLeft failureMsg else return ()

seqDo :: [FailableGameAction a] -> FailableGameAction ()
seqDo = sequence_

doNTimes :: Int -> FailableGameAction a -> FailableGameAction ()
doNTimes n action = seqDo $ map (\_ -> action) $ (replicate n () :: [()])

playerHasCards :: PlayerIndex -> [Card] -> GameState -> Bool
playerHasCards playerInd cards gs = case getPlayer playerInd gs of
  Nothing     -> False
  Just player -> all (`elem` (_playerHand player)) cards

addCardStacksToHarvestPile
  :: PlayerIndex -> [CardStack] -> FailableGameAction ()
addCardStacksToHarvestPile playerIndex stacks = do
  let cardsInStacks = (\(CardStack _ cards) -> cards) =<< stacks
  updateState $ mapPlayerM
    playerIndex
    (\prevPlayer -> return $ prevPlayer
      { _playerHarvestPile = (cardsInStacks ++ _playerHarvestPile prevPlayer)
      }
    )

removeCardsFromPlayersHand :: PlayerIndex -> [Card] -> FailableGameAction ()
removeCardsFromPlayersHand playerIndex cards = do
  playerHasCards' <- withS $ playerHasCards playerIndex cards
  checkS playerHasCards' "Player doesn't have those cards" -- TODO is this necessary?
  updateState $ mapPlayerM
    playerIndex
    (\prevPlayer -> return $ prevPlayer
      { _playerHand = (_playerHand prevPlayer) `setSubtract` cards
      }
    )

givePlayerOkus :: PlayerIndex -> FailableGameAction ()
givePlayerOkus playerIndex = do
  prevNumOkuses <- withS (_illNumOkuses . _gameIllimatState)
  if prevNumOkuses > 0
    then do
      gs <- get
      updateState $ mapPlayerM
        playerIndex
        (\ps -> return ps { _playerNumOkuses = 1 + _playerNumOkuses ps })
      updateState $ return . mapIllimat
        (\is -> is { _illNumOkuses = (_illNumOkuses is) - 1 })
    else returnLeft "No more okuses!"


resolveSeasonChange :: Card -> Direction -> FailableGameAction ()
resolveSeasonChange (Card val cseason) dir =
  if val `elem` [Fool, Knight, Queen, King]
    then case cseason of
      CSummer -> doAllowingFailure (setSeason Summer dir) >> return ()
      CAutumn -> doAllowingFailure (setSeason Autumn dir) >> return ()
      CWinter -> doAllowingFailure (setSeason Winter dir) >> return ()
      CSpring -> doAllowingFailure (setSeason Spring dir) >> return ()
      CStars  -> return ()
    else return ()

doInitialGameSetup :: FailableGameAction ()
doInitialGameSetup = do
  seqDo (map dealLuminaryToField (allEnum :: [Direction]))
  seqDo (map deal3CardsToField (allEnum :: [Direction]))
  currPlayers <- withS _gamePlayerState
  seqDo (map (\(_, i) -> fillPlayersHandTo4 i) $ zip currPlayers [0 ..])

fillPlayersHandTo4 :: PlayerIndex -> FailableGameAction ()
fillPlayersHandTo4 playerIndex = do
  currPlayer <- getPlayerS playerIndex
  doNTimes (4 - (length $ _playerHand currPlayer))
           (doAllowingFailure $ dealCardToPlayer playerIndex)

stockpile
  :: PlayerIndex
  -> Card
  -> Direction
  -> [CardStack]
  -> Int
  -> FailableGameAction ()
stockpile playerIndex card dir targetStacks desiredStackVal = do
  currPlayer <- getPlayerS playerIndex
  checkS (card `elem` (_playerHand currPlayer))
         "Player doesn't have the card they're trying to play!"
  let stackIsValid =
        canHarvestWithNum desiredStackVal ((fromCard card) : targetStacks)
  checkS stackIsValid
         "Attempted stack is invalid (value doesn't match included cards)"
  let resultingStack = CardStack
        [desiredStackVal]
        (card : ((\(CardStack _ cards) -> cards) =<< targetStacks))
  checkS
    (any (\handCard -> [handCard] `cardsCanHarvest` [resultingStack])
         ((_playerHand currPlayer) `setDelete` card)
    )
    "Player doesn't have any cards that can harvest the new stockpile"
  (_, season) <- withS $ fieldSFromDir dir
  checkNotS (season == Spring) "Can't stockpile because it's spring!"

  -- All checks passed
  removeCardsFromPlayersHand playerIndex [card]
  seqDo $ map (\stack -> removeCardStackFromField stack dir) targetStacks
  updateState $ mapFieldM
    dir
    (\fs -> return $ fs { _fieldCards = resultingStack : (_fieldCards fs) })
  resolveSeasonChange card dir

sow :: Bool -> PlayerIndex -> Card -> Direction -> FailableGameAction ()
sow ignoreAutumnForTheRake playerIndex card dir = do
  currPlayer <- getPlayerS playerIndex
  checkS (card `elem` (_playerHand currPlayer))
         "Player doesn't have the card they tried to sow!"
  (_, season) <- withS $ fieldSFromDir dir
  checkNotS (season == Autumn && (not ignoreAutumnForTheRake))
            "Can't sow because it's autumn!"
  removeCardsFromPlayersHand playerIndex [card]
  updateState $ mapFieldM
    dir
    (\fs -> return fs { _fieldCards = (fromCard card) : (_fieldCards fs) })
  resolveSeasonChange card dir

harvest
  :: PlayerIndex -> [Card] -> Direction -> [CardStack] -> FailableGameAction ()
harvest playerIndex playedCards fieldDir targetStacks = do
  stacksAreInField <- tryAll
    (map (\stack -> removeCardStackFromField stack fieldDir) targetStacks)
  currSeason <- withS $ seasonFromDir fieldDir
  currGS     <- get
  let maidenIsActive = (FaceUp Maiden) `elem` luminaries
      unionIsInField =
        (FaceUp Union) == (_fieldLuminary $ fieldFromDir fieldDir currGS)
      luminaries = map _fieldLuminary fields
      fields = map (\dir -> fieldFromDir dir currGS) (allEnum :: [Direction])
      playerHasPlayedCards = playerHasCards playerIndex playedCards currGS
  checkS
    (      length playedCards
    `elem` [1, 4]
    ||     unionIsInField
    &&     length playedCards
    ==     2
    )
    "Harvested with the wrong number of cards!"
  checkNotS (currSeason == Winter && (not maidenIsActive))
            "Can't harvest because it's winter!"
  checkS stacksAreInField
         "Can't harvest because target stacks aren't in the field!"
  checkS playerHasPlayedCards
         "Can't harvest because player doesn't have the cards they're using!"
  checkS (cardsCanHarvest playedCards targetStacks)
         "The played cards can't harvest the targeted cards."

  -- all checks passed; remove cards from player's hand, 
  -- and put them & the targeted cards in their harvest stack
  removeCardsFromPlayersHand playerIndex playedCards
  seqDo (map (\stack -> removeCardStackFromField stack fieldDir) targetStacks)
  addCardStacksToHarvestPile playerIndex targetStacks
  addCardStacksToHarvestPile playerIndex (map fromCard playedCards)

  seqDo $ map (\card -> resolveSeasonChange card fieldDir) playedCards

  -- re-fill player's hand up to 4
  resultingPlayer <- getPlayerS playerIndex
  doNTimes (4 - (length $ _playerHand resultingPlayer))
           (doAllowingFailure $ dealCardToPlayer playerIndex)

  -- check if player has cleared the field
  resultingFieldState <- withS $ fieldFromDir fieldDir
  let fieldNowEmpty = null $ _fieldCards resultingFieldState
  if fieldNowEmpty
    then do
    -- try to give okus
      gaveOkus                 <- doAllowingFailure $ givePlayerOkus playerIndex

      -- resolve luminary
      fieldHadFaceDownLuminary <- case (_fieldLuminary resultingFieldState) of
        FaceDown lum -> do
          succeededInResolvingLuminary <-
            doAllowingFailure $ resolveLuminaryReveal lum fieldDir
          if not succeededInResolvingLuminary
            then
            -- discard the luminary
                 updateState $ mapFieldM
              fieldDir
              (\fs -> return $ fs { _fieldLuminary = NoLuminary })
            else
            -- flip it face-up
                 updateState $ mapFieldM
              fieldDir
              (\fs -> return $ fs { _fieldLuminary = FaceUp lum })
          return True
        FaceUp lum -> do
          updateState $ mapFieldM
            fieldDir
            (\fs -> return $ fs { _fieldLuminary = NoLuminary })
          updateState $ mapPlayerM
            playerIndex
            (\ps ->
              return $ ps { _playerLuminaries = lum : (_playerLuminaries ps) }
            )
          resolveLuminaryTake lum playerIndex
          return False
        NoLuminary -> return False

      if gaveOkus && (not fieldHadFaceDownLuminary) -- let luminaries resolve re-seeding on reveal
        then (doAllowingFailure $ deal3CardsToField fieldDir) >> return ()
        else return ()
    else return ()

setSeason :: Season -> Direction -> FailableGameAction ()
setSeason Summer dir = do
  currGS <- get
  -- check if forest queen is anywhere
  let forestQueenIsActive = (FaceUp Forest_Queen) `elem` luminaries
      luminaries = map _fieldLuminary fields
      fields = map (\dir' -> fieldFromDir dir' currGS) (allEnum :: [Direction])
      willBeSettingSummerForForestQueen =
        (FaceUp Forest_Queen) == (_fieldLuminary $ fieldFromDir dir currGS)
  if forestQueenIsActive && not willBeSettingSummerForForestQueen
    then returnLeft "Can't change seasons; Forest Queen is active."
    else updateState $ mapIllimatM (\is -> return is { _illSummerDir = dir })
setSeason notSummer dir = setSeason (succ notSummer) (succ dir)

-- https://www.illimat.com/rulesclarifications/2017/11/27/clearing-and-refilling-fields
resolveLuminaryReveal :: Luminary -> Direction -> FailableGameAction ()
resolveLuminaryReveal lum dirLumWasIn =
  let defaultBehavior = deal3CardsToField dirLumWasIn
  in
    case lum of
      Union        -> defaultBehavior
      Maiden       -> defaultBehavior
      Rake         -> defaultBehavior
      Changeling   -> defaultBehavior
      Forest_Queen -> (setSeason Summer dirLumWasIn) >> defaultBehavior
      River        -> dealUntilFieldHasNCards 6 dirLumWasIn
      Newborn      -> do
        defaultBehavior
        let oppDir = succ $ succ $ dirLumWasIn
        currOppField <- getFieldS oppDir
        case (_fieldLuminary currOppField) of
          FaceDown oppLum -> do
            updateState $ mapFieldM
              oppDir
              (\fs -> return $ fs { _fieldLuminary = FaceUp oppLum })
            resolveLuminaryReveal oppLum oppDir
          FaceUp _   -> return ()
          NoLuminary -> do
            remainingLuminaries <- withS _gameUnusedLuminaries
            case remainingLuminaries of
              h : tl ->
                (doAllowingFailure $ do
                    resolveLuminaryReveal h oppDir
                    updateState $ mapFieldM
                      oppDir
                      (\fs -> return $ fs { _fieldLuminary = FaceUp h })
                  )
                  >> (return ())
              _ -> return ()
      Children -> do
        currGS <- get
        case (_gameDeck currGS) of
          a : b : c : rest -> do
            put $ currGS { _gameDeck = rest, _gameChildrenCards = [a, b, c] }
            defaultBehavior
          _ -> returnLeft "Not enough cards to reserve for the Children!"

resolveLuminaryTake :: Luminary -> PlayerIndex -> FailableGameAction ()
resolveLuminaryTake lum playerIndex = case lum of
  Rake -> do
    let
      giveCardFromAToB card aInd bInd = do
        updateState $ mapPlayerM
          aInd
          (\ps -> return $ ps
            { _playerHarvestPile = (_playerHarvestPile ps) `setDelete` card
            }
          )
        updateState $ mapPlayerM
          bInd
          (\ps -> return
            $ ps { _playerHarvestPile = card : (_playerHarvestPile ps) }
          )
      filterForNonFoolSummer =
        filter $ \(Card val season) -> season == CSummer && val /= Fool
      filterForSummer = filter $ \(Card _ season) -> season == CSummer
      grabSummerCardFrom ind = do
        targetPlayer <- getPlayerS ind
        case filterForNonFoolSummer (_playerHand targetPlayer) of
          [] -> case filterForSummer (_playerHand targetPlayer) of
            []     -> return ()
            h : tl -> giveCardFromAToB h ind playerIndex
          h : tl -> giveCardFromAToB h ind playerIndex
    numPlayers <- withS $ (length . _gamePlayerState)
    seqDo $ map grabSummerCardFrom
                (filter (/= playerIndex) (take numPlayers [0 ..]))
  Children -> do
    currChildrenCards <- withS _gameChildrenCards
    updateState (\gs -> return $ gs { _gameChildrenCards = [] })
    updateState $ mapPlayerM
      playerIndex
      (\ps -> return $ ps
        { _playerHarvestPile = currChildrenCards ++ (_playerHarvestPile ps)
        }
      )
  _ -> return ()


cardsCanHarvest :: [Card] -> [CardStack] -> Bool
cardsCanHarvest cardsUsed targetStacks = any
  (\handSum -> canHarvestWithNum handSum targetStacks)
  possibleHandSums
 where
  possibleHandSums = allPossibleSums [0] cardsUsed
  allPossibleSums acc ((Card cardVal _) : tl) =
    let acc' = (+) <$> (toNumberVals cardVal) <*> acc
    in  allPossibleSums acc' tl
  allPossibleSums acc [] = acc

canHarvestWithNum :: Int -> [CardStack] -> Bool
canHarvestWithNum num stacks = duplicateStacksMatchNum && additionsWork
 where
  duplicateStacksMatchNum = all
    (\(CardStack vals cards) -> (length cards == 1) || num `elem` vals)
    stacks
  additionsWork =
    any (\interp -> canFormPartition num interp) stackInterpretations
  stackInterpretations = foldr (\numVals accls -> (:) <$> numVals <*> accls)
                               [[]]
                               (map (\(CardStack vals _) -> vals) stacks)

allSubsets :: [a] -> [[a]]
allSubsets []       = [[]]
allSubsets (h : tl) = ((h :) <$> (allSubsets tl)) ++ (allSubsets tl)

allSubsetsSummingTo :: Int -> [Int] -> [[Int]]
allSubsetsSummingTo n ls = filter ((== n) . sum) (allSubsets ls)

setDelete :: (Eq a) => [a] -> a -> [a]
[] `setDelete` a = []
(x : xs) `setDelete` a | x == a    = xs
                       | otherwise = x : (xs `setDelete` a)

setSubtract :: (Eq a) => [a] -> [a] -> [a]
as `setSubtract` []       = as
as `setSubtract` (b : bs) = (as `setDelete` b) `setSubtract` bs

canFormPartition :: Int -> [Int] -> Bool
canFormPartition _ [] = True
canFormPartition partitionSum (h : tl)
  | h == partitionSum
  = canFormPartition partitionSum tl
  | h > partitionSum
  = False
  | otherwise
  = let subsets = (allSubsetsSummingTo (partitionSum - h) tl)
        trySubset subset =
            canFormPartition partitionSum (tl `setSubtract` subset)
    in  any trySubset subsets

toNumberVals :: CardVal -> [Int]
toNumberVals Fool     = [1, 14]
toNumberVals otherVal = [toNumberValWithFoolAsOne otherVal]

toNumberValWithFoolAsOne :: CardVal -> Int
toNumberValWithFoolAsOne Fool = 1
toNumberValWithFoolAsOne otherVal =
  1 + (toNumberValWithFoolAsOne $ pred otherVal)

toNumberValWithFoolAsFourteen :: CardVal -> Int
toNumberValWithFoolAsFourteen Two = 2
toNumberValWithFoolAsFourteen otherVal =
  1 + (toNumberValWithFoolAsFourteen $ pred otherVal)



-- scoring
-- score :: [PlayerState] -> [Int]
-- score players =
--   let maxIndexSet :: (PlayerState -> PlayerState -> Ordering) -> [PlayerState] -> [PlayerIndex]
--       maxIndexSet cmp ls = map snd $ headOr [] $ groupSortBy paircmp (:) $ zip ls [0..]
--         where paircmp (a, i) (b, j) = cmp a b
--       biggerFirst :: (a -> Int) -> a -> a -> Ordering
--       biggerFirst f a b
--         | (f a) > (f b) = LT
--         | (f a) < (f b) = GT
--         | otherwise = EQ
--       tiebreak :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
--       tiebreak f g a b = case f a b of
--         EQ -> g a b
--         _ -> f a b
--       invert :: (a -> a -> Ordering) -> a -> a -> Ordering
--       invert f a b = swapLTGT $ f a b
--         where swapLTGT LT = GT
--               swapLTGT GT = LT
--               swapLTGT EQ = EQ
--       byHasRiver :: (PlayerState -> PlayerState -> Ordering)
--       byHasRiver = biggerFirst $ boolToInt . (River `elem`) . _playerLuminaries
--       byLuminaries :: (PlayerState -> PlayerState -> Ordering)
--       byLuminaries = biggerFirst countLuminaries
--       byMostCards :: (PlayerState -> PlayerState -> Ordering)
--       byMostCards = (biggerFirst (length . _playerHarvestPile)) `tiebreak` byLuminaries
--       byMostSummers :: (PlayerState -> PlayerState -> Ordering)
--       byMostSummers = (biggerFirst (sum . (map pointForSummer) . _playerHarvestPile)) `tiebreak` byLuminaries
--         where pointForSummer (Card _ CSummer) = 1
--               pointForSummer _ = 0
--       byMostWinters = (biggerFirst (sum . (map pointForWinter) . _playerHarvestPile)) `tiebreak` byHasRiver `tiebreak` (invert byLuminaries)
--         where pointForWinter (Card _ CWinter) = 1
--               pointForWinter _ = 0
--       onlyAllowOne :: [a] -> Maybe a
--       onlyAllowOne [a] = Just a
--       onlyAllowOne _ = Nothing
--       countFools :: PlayerState -> Int
--       countFools = sum . (map pointForFool) . _playerHarvestPile
--         where pointForFool (Card Fool _) = 1
--               pointForFool _ = 0
--       countLuminaries :: PlayerState -> Int
--       countLuminaries = length . _playerLuminaries
--   in  foldr joinScore (map $ const 0 $ players) scoreSpecs
--         where scoreSpecs = [ (byMostCards, const 4)
--                            , (byMostSummers, const 2)
--                            , (byMostWinters, (\ps -> if River `elem` (_playerLuminaries ps) then 2 else -2))
--                            ]
--               joinScore :: (PlayerState -> PlayerState -> Ordering, PlayerState -> PlayerIndex) -> [Int] -> [Int]


-- ToJSON generation for elm connection
-- myOptions = Elm.Derive.defaultOptions
--   { fieldLabelModifier = \s -> if take 1 s == "_" then drop 1 s else s
--   }

$(deriveBoth myOptions ''GameState)
$(deriveBoth myOptions ''IllimatState)
$(deriveBoth myOptions ''Direction)
$(deriveBoth myOptions ''LuminaryState)
$(deriveBoth myOptions ''BoardState)
$(deriveBoth myOptions ''FieldState)
$(deriveBoth myOptions ''CardStack)
$(deriveBoth myOptions ''Card)
$(deriveBoth myOptions ''CardVal)
$(deriveBoth myOptions ''CardSeason)
$(deriveBoth myOptions ''PlayerState)
$(deriveBoth myOptions ''Luminary)


makeLenses ''GameState
makeLenses ''IllimatState
makeLenses ''BoardState
makeLenses ''FieldState
makeLenses ''PlayerState
makeLenses ''Card
