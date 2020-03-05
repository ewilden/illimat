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

(!!) :: [a] -> Int -> Maybe a
[] !! _ = Nothing
(h:tl) !! 0 = Just h
(h:tl) !! n
    | n < 0 = Nothing
    | otherwise = tl !! (n - 1)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GameState = GameState 
    { gameIllimatState :: IllimatState
    , gameBoardState :: BoardState
    , gamePlayerState :: [PlayerState]
    , gameDeck :: Deck
    , gameUnusedLuminaries :: [Luminary]
    , gameChildrenCards :: [Card]
    } deriving (Show)

boxIdent :: (a -> a) -> (a -> Identity a)
boxIdent f = Identity . f

unboxIdent :: (a -> Identity a) -> (a -> a)
unboxIdent f = (\(Identity a) -> a) . f

mapIllimat :: (IllimatState -> IllimatState) -> GameState -> GameState
mapIllimat f gs = gs { gameIllimatState = f $ gameIllimatState gs }

mapIllimatM :: (Monad m) => (IllimatState -> m IllimatState) -> GameState -> m GameState
mapIllimatM f gs = (\is -> gs { gameIllimatState = is}) <$> (f $ gameIllimatState gs)

mapBoard :: (BoardState -> BoardState) -> GameState -> GameState
mapBoard = unboxIdent . mapBoardM . boxIdent

mapBoardM :: (Monad m) => (BoardState -> m BoardState) -> GameState -> m GameState
mapBoardM f gs = (\bs -> gs { gameBoardState = bs }) <$> (f $ gameBoardState gs)

mapField :: Direction -> (FieldState -> FieldState) -> GameState -> GameState
mapField dir f = unboxIdent (mapFieldM dir (boxIdent f))

mapFieldM :: (Monad m) => Direction -> (FieldState -> m FieldState) -> GameState -> m GameState
mapFieldM dir f gs = do
  let targetedField = fieldFromDir dir gs
  nextField <- f targetedField
  return $ fieldPutter dir gs nextField

mapPlayerState :: ([PlayerState] -> [PlayerState]) -> GameState -> GameState
mapPlayerState f gs = gs { gamePlayerState = f $ gamePlayerState gs }

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
fieldSFromDir dir gs = (fieldGetter N $ (gameBoardState gs), seasonFromDir dir gs)

fieldGetter :: Direction -> BoardState -> FieldState
fieldGetter N = bsFieldN
fieldGetter S = bsFieldS
fieldGetter E = bsFieldE
fieldGetter W = bsFieldW

fieldPutter :: Direction -> GameState -> (FieldState -> GameState)
fieldPutter N gs fs = mapBoard (\bs -> bs { bsFieldN = fs}) gs
fieldPutter S gs fs = mapBoard (\bs -> bs { bsFieldS = fs}) gs
fieldPutter E gs fs = mapBoard (\bs -> bs { bsFieldE = fs}) gs
fieldPutter W gs fs = mapBoard (\bs -> bs { bsFieldW = fs}) gs

fieldFromDir :: Direction -> GameState -> FieldState
fieldFromDir dir = fst . (fieldSFromDir dir)

getFieldS :: Direction -> FailableGameAction FieldState
getFieldS dir = do
  currGS <- get
  return $ fieldFromDir dir currGS

seasonFromDir :: Direction -> GameState -> Season
seasonFromDir dir gs
    | dir == (illSummerDir $ gameIllimatState gs) = Summer
    | otherwise = pred $ seasonFromDir (succ dir) gs

enumStartingFrom :: (Enum a, Bounded a, Eq a) => a -> [a]
enumStartingFrom startVal = 
    if startVal == maxBound 
    then [startVal] 
    else startVal : enumStartingFrom (succ startVal)

enumWheel :: (Enum a, Bounded a, Eq a) => a -> [a]
enumWheel a = a : (enumWheel $ succ a)

type Deck = [Card]
type LuminaryDeck = [Luminary]

allEnum :: (Enum a, Bounded a, Eq a) => [a]
allEnum = enumStartingFrom minBound

allCards :: Deck
allCards = Card <$> (allEnum :: [CardVal]) <*> (allEnum :: [CardSeason])

allCardsMinusStars :: Deck
allCardsMinusStars = filter notStar allCards
    where notStar (Card _ CStars) = False
          notStar _ = True

allLuminaries :: LuminaryDeck
allLuminaries = allEnum

emptyGameState :: Int -> Direction -> Deck -> LuminaryDeck -> GameState
emptyGameState numPlayers initSummerDir startingDeck startingLuminaryDeck =
    GameState
    { gameIllimatState = IllimatState initSummerDir numPlayers
    , gameBoardState = emptyBoard
    , gamePlayerState = replicate numPlayers emptyPlayer
    , gameDeck = startingDeck
    , gameUnusedLuminaries = startingLuminaryDeck
    , gameChildrenCards = []
    }
    where
        emptyBoard = BoardState emptyField emptyField emptyField emptyField
        emptyPlayer = PlayerState [] 0 [] []
        emptyField = FieldState [] NoLuminary

twoPlayerDefaultState :: GameState
twoPlayerDefaultState = emptyGameState 2 N allCardsMinusStars allLuminaries

type PlayerIndex = Int
type GameStateM = State GameState

patchListAt :: Int -> (a -> a) -> [a] -> Maybe [a]
patchListAt n f l = case splitAt n l of
    (pre, x:post) -> Just $ pre ++ ((f x):post)
    _ -> Nothing

-- <&> = flip <$>

patchListEith :: s -> Int -> (a -> Either s a) -> [a] -> Either s [a]
patchListEith failMsg ind f ls = case splitAt ind ls of
  (pre, x:post) -> (f x) <&> (: post) <&> (pre ++)
  _ -> Left failMsg

type FailableGameAction a = StateT GameState (Either String) a

toEith :: s -> Maybe a -> Either s a
toEith _ (Just a) = Right a
toEith s Nothing = Left s

dealCardToPlayer :: PlayerIndex -> FailableGameAction ()
dealCardToPlayer playerIndex = StateT $ \gameState ->
    do
        topCard <- toEith "No more cards!" $ headMay $ gameDeck gameState
        restOfDeck <- toEith "No more cards!" $ tailMay $ gameDeck gameState
        nextPlayers <- toEith "Wrong number of players" $ patchListAt playerIndex (mapHand (topCard :)) $ gamePlayerState gameState
        return (
                (),
                gameState {
                    gamePlayerState = nextPlayers
                    , gameDeck = restOfDeck
                }
            )

dealUntilFieldHasNCards :: Int -> Direction -> FailableGameAction ()
dealUntilFieldHasNCards n fieldDir = do
  currGS <- get
  let currDeck = gameDeck currGS
      currField = fieldFromDir fieldDir currGS
      cardsToDeal = max 0 (n - (length $ fieldCards currField))
  if
    (length currDeck) >= cardsToDeal
  then do
    updateState $ mapFieldM fieldDir (\fs -> return $ fs {fieldCards = (fromCard <$> (take cardsToDeal currDeck))})
    updateState $ (\gs -> return $ gs {gameDeck = drop cardsToDeal currDeck})
  else
    returnLeft "Didn't have enough cards to re-seed the field!"

deal3CardsToField :: Direction -> FailableGameAction ()
deal3CardsToField fieldDir = dealUntilFieldHasNCards 3 fieldDir

doAllowingFailure :: FailableGameAction () -> FailableGameAction Bool
doAllowingFailure action = do
  succeeded <- try' action
  case succeeded of 
    True -> action >> (return True)
    False -> return False
 
mapHand :: ([Card] -> [Card]) -> PlayerState -> PlayerState
mapHand f (PlayerState hand okuses luminaries pile) = 
    PlayerState (f hand) okuses luminaries pile

getPlayer :: PlayerIndex -> GameState -> Maybe PlayerState
getPlayer ind gs = ((!! ind) . gamePlayerState) gs

getPlayerS :: PlayerIndex -> FailableGameAction PlayerState
getPlayerS ind = do
  currGS <- get
  case getPlayer ind currGS of
    Nothing -> returnLeft "That player doesn't exist!"
    Just player -> return player

returnLeft :: a -> StateT GameState (Either a) b
returnLeft a = StateT (\_ -> Left a)

removeCardStackFromField :: CardStack -> Direction -> FailableGameAction ()
removeCardStackFromField cardStack dir = do
    currGS <- get
    let field = fieldFromDir dir currGS
    case cardStack `elem` (fieldCards field) of
        True -> do
                    put $ mapField dir 
                      (\(FieldState cards luminary) -> 
                          FieldState (filter (/= cardStack) cards) luminary) currGS
                    return ()
        False -> returnLeft "Card stack not in field!"

wouldSucceed :: GameState -> FailableGameAction a -> Bool
wouldSucceed s action = case (runStateT action s) of
    Right _ -> True
    Left _ -> False

try' :: FailableGameAction a -> FailableGameAction Bool
try' action = do
                s <- get
                return $ wouldSucceed s action

tryAll :: [FailableGameAction a] -> FailableGameAction Bool
tryAll actions = (all id) <$> (sequence (map try' actions))

fromCard :: Card -> CardStack
fromCard card@(Card val season) = CardStack (toNumberVals val) [card]

removeCardFromField :: Card -> Direction -> FailableGameAction ()
removeCardFromField card dir = removeCardStackFromField (fromCard card) dir

withS :: (GameState -> a) -> FailableGameAction a
withS f = do
            s <- get
            return $ f s

updateStatePure :: (GameState -> GameState) -> FailableGameAction ()
updateStatePure f = updateState (return . f)

updateState :: (GameState -> Either String GameState) -> FailableGameAction ()
updateState f = do
  s <- get
  case f s of
    Left errMsg -> returnLeft errMsg
    Right gs -> put gs

liftPlayerS :: PlayerIndex -> (PlayerState -> Either String PlayerState) -> (GameState -> Either String GameState)
liftPlayerS playerInd f gs =
  let
    prevPlayers = gamePlayerState gs
    patchResult = patchListEith "no such player" playerInd f prevPlayers
  in
    (\nextPlayers -> gs { gamePlayerState = nextPlayers }) <$> patchResult

checkS :: Bool -> String -> FailableGameAction ()
checkS cond failureMsg = if cond then return () else returnLeft failureMsg

checkNotS :: Bool -> String -> FailableGameAction ()
checkNotS failCond failureMsg = if failCond then returnLeft failureMsg else return ()

seqDo :: (Monad m) => [m a] -> m ()
seqDo actions = do
  _ <- sequence actions
  return ()

doNTimes :: (Monad m) => Int -> m a -> m ()
doNTimes n action = seqDo $ map (\_ -> action) $ replicate n ()

playerHasCards :: PlayerIndex -> [Card] -> GameState -> Bool
playerHasCards playerInd cards gs =
  case getPlayer playerInd gs of
    Nothing -> False
    Just player -> all (`elem` (playerHand player)) cards

addCardStacksToHarvestPile :: PlayerIndex -> [CardStack] -> FailableGameAction ()
addCardStacksToHarvestPile playerIndex stacks = do
  let cardsInStacks = (\(CardStack _ cards) -> cards) =<< stacks
  updateState $ liftPlayerS playerIndex
    (\prevPlayer -> return $ 
      prevPlayer {playerHarvestPile = (cardsInStacks ++ playerHarvestPile prevPlayer)})

removeCardsFromPlayersHand :: PlayerIndex -> [Card] -> FailableGameAction ()
removeCardsFromPlayersHand playerIndex cards = do
  playerHasCards' <- withS $ playerHasCards playerIndex cards
  checkS playerHasCards' "Player doesn't have those cards" -- TODO is this necessary?
  updateState $ liftPlayerS playerIndex
    (\prevPlayer -> return $
      prevPlayer {playerHand = (playerHand prevPlayer) `setSubtract` cards})

givePlayerOkus :: PlayerIndex -> FailableGameAction ()
givePlayerOkus playerIndex = do
  prevNumOkuses <- withS (illNumOkuses . gameIllimatState)
  if 
    prevNumOkuses > 0
  then do
    gs <- get
    updateState $ liftPlayerS playerIndex (\ps -> return ps { playerNumOkuses = 1 + playerNumOkuses ps})
    updateState $ return . mapIllimat (\is -> is { illNumOkuses = (illNumOkuses is) - 1})
  else
    returnLeft "No more okuses!"


resolveSeasonChange :: Card -> Direction -> FailableGameAction ()
resolveSeasonChange (Card val cseason) dir = do
  if 
    val `elem` [Fool, Knight, Queen, King]
  then
    case cseason of 
      CSummer -> (doAllowingFailure $ setSeason Summer dir) >> return ()
      CAutumn -> (doAllowingFailure $ setSeason Autumn dir) >> return ()
      CWinter -> (doAllowingFailure $ setSeason Winter dir) >> return ()
      CSpring -> (doAllowingFailure $ setSeason Spring dir) >> return ()
      CStars -> return ()
  else
    return ()



stockpile :: PlayerIndex -> Card -> Direction -> [CardStack] -> Int -> FailableGameAction ()
stockpile playerIndex card dir targetStacks desiredStackVal = do
  currPlayer <- getPlayerS playerIndex
  checkS (card `elem` (playerHand currPlayer)) 
    "Player doesn't have the card they're trying to play!"
  let stackIsValid = canHarvestWithNum desiredStackVal ((fromCard card):targetStacks)
  checkS stackIsValid 
    "Attempted stack is invalid (value doesn't match included cards)"
  let resultingStack = CardStack [desiredStackVal] (card : ((\(CardStack _ cards) -> cards) =<< targetStacks))
  checkS (any 
    (\handCard -> [handCard] `cardsCanHarvest` [resultingStack]) 
    ((playerHand currPlayer) `setDelete` card)) 
    "Player doesn't have any cards that can harvest the new stockpile"
  (_, season) <- withS $ fieldSFromDir dir
  checkNotS (season == Spring) "Can't stockpile because it's spring!"

  -- All checks passed
  removeCardsFromPlayersHand playerIndex [card]
  seqDo $ map (\stack -> removeCardStackFromField stack dir) targetStacks
  updateState $ mapFieldM dir (\fs -> return $ fs { fieldCards = resultingStack : (fieldCards fs) })  
  resolveSeasonChange card dir

sow :: Bool -> PlayerIndex -> Card -> Direction -> FailableGameAction ()
sow ignoreAutumnForTheRake playerIndex card dir = do
  currPlayer <- getPlayerS playerIndex
  checkS (card `elem` (playerHand currPlayer)) 
    "Player doesn't have the card they tried to sow!"
  (_, season) <- withS $ fieldSFromDir dir
  checkNotS (season == Autumn && (not ignoreAutumnForTheRake)) "Can't sow because it's autumn!"
  removeCardsFromPlayersHand playerIndex [card]
  updateState $ mapFieldM dir (\fs -> return fs {fieldCards = (fromCard card) : (fieldCards fs)})
  resolveSeasonChange card dir

harvest :: PlayerIndex -> [Card] -> Direction -> [CardStack] -> FailableGameAction ()
harvest playerIndex playedCards fieldDir targetStacks = do
  stacksAreInField <- tryAll (map (\stack -> removeCardStackFromField stack fieldDir) targetStacks)
  currSeason <- withS $ seasonFromDir fieldDir
  currGS <- get
  let maidenIsActive = (FaceUp Maiden) `elem` luminaries
          where luminaries = map fieldLuminary fields
                fields = map (\dir -> fieldFromDir dir currGS) (allEnum :: [Direction])
      playerHasPlayedCards = playerHasCards playerIndex playedCards currGS
  checkNotS (currSeason == Winter && (not maidenIsActive)) "Can't harvest because it's winter!"
  checkS stacksAreInField "Can't harvest because target stacks aren't in the field!"
  checkS playerHasPlayedCards "Can't harvest because player doesn't have the cards they're using!"
  checkS (cardsCanHarvest playedCards targetStacks) "The played cards can't harvest the targeted cards."

  -- all checks passed; remove cards from player's hand, 
  -- and put them & the targeted cards in their harvest stack
  removeCardsFromPlayersHand playerIndex playedCards
  seqDo (map (\stack -> removeCardStackFromField stack fieldDir) targetStacks)
  addCardStacksToHarvestPile playerIndex targetStacks
  addCardStacksToHarvestPile playerIndex (map fromCard playedCards)
  
  seqDo $ map (\card -> resolveSeasonChange card fieldDir) playedCards

  -- re-fill player's hand up to 4
  resultingPlayer <- getPlayerS playerIndex
  doNTimes 
    (4 - (length $ playerHand resultingPlayer))
    (doAllowingFailure $ dealCardToPlayer playerIndex)

  -- check if player has cleared the field
  resultingFieldState <- withS $ fieldFromDir fieldDir
  let fieldNowEmpty = null $ fieldCards resultingFieldState
  if
    fieldNowEmpty
  then do
    -- try to give okus
    gaveOkus <- doAllowingFailure $ givePlayerOkus playerIndex

    -- resolve luminary
    fieldHadFaceDownLuminary <- case (fieldLuminary resultingFieldState) of
      FaceDown lum -> do
        succeededInResolvingLuminary <- doAllowingFailure $ resolveLuminaryReveal lum fieldDir
        if 
          not succeededInResolvingLuminary
        then
          -- discard the luminary
          updateState $ mapFieldM fieldDir (\fs -> return $ fs {fieldLuminary = NoLuminary})
        else
          -- flip it face-up
          updateState $ mapFieldM fieldDir (\fs -> return $ fs {fieldLuminary = FaceUp lum})
        return True
      FaceUp lum -> do
        updateState $ mapFieldM fieldDir (\fs -> return $ fs {fieldLuminary = NoLuminary})
        updateState $ liftPlayerS playerIndex (\ps -> return $ ps { playerLuminaries = lum : (playerLuminaries ps)})
        resolveLuminaryTake lum playerIndex
        return False
      NoLuminary -> return False

    if 
      gaveOkus && (not fieldHadFaceDownLuminary) -- let luminaries resolve re-seeding on reveal
    then
      (doAllowingFailure $ deal3CardsToField fieldDir) >> return ()
    else 
      return ()
    
  else
    return ()

setSeason :: Season -> Direction -> FailableGameAction ()
setSeason Summer dir = do
  currGS <- get
  -- check if forest queen is anywhere
  let forestQueenIsActive = (FaceUp Forest_Queen) `elem` luminaries
      luminaries = map fieldLuminary fields
      fields = map (\dir' -> fieldFromDir dir' currGS) (allEnum :: [Direction])
      willBeSettingSummerForForestQueen =
        (FaceUp Forest_Queen) == (fieldLuminary $ fieldFromDir dir currGS)
  if 
    forestQueenIsActive && not willBeSettingSummerForForestQueen
  then
    returnLeft "Can't change seasons; Forest Queen is active."
  else
    updateState $ mapIllimatM (\is -> return is {illSummerDir = dir})
setSeason notSummer dir = setSeason (succ notSummer) (succ dir)

-- https://www.illimat.com/rulesclarifications/2017/11/27/clearing-and-refilling-fields
resolveLuminaryReveal :: Luminary -> Direction -> FailableGameAction ()
resolveLuminaryReveal lum dirLumWasIn = 
  let defaultBehavior = deal3CardsToField dirLumWasIn in
    case lum of
      Union -> defaultBehavior
      Maiden -> defaultBehavior
      Rake -> defaultBehavior
      Changeling -> defaultBehavior
      Forest_Queen -> (setSeason Summer dirLumWasIn) >> defaultBehavior
      River -> dealUntilFieldHasNCards 6 dirLumWasIn
      Newborn -> do
        defaultBehavior
        let oppDir = succ $ succ $ dirLumWasIn
        currOppField <- getFieldS oppDir
        case (fieldLuminary currOppField) of
          FaceDown oppLum -> do
            updateState $ mapFieldM oppDir (\fs -> return $ fs {fieldLuminary = FaceUp oppLum})
            resolveLuminaryReveal oppLum oppDir
          FaceUp _ -> return ()
          NoLuminary -> do
            remainingLuminaries <- withS gameUnusedLuminaries
            case remainingLuminaries of
              h:tl -> (doAllowingFailure $ do 
                  resolveLuminaryReveal h oppDir
                  updateState $ mapFieldM oppDir (\fs -> return $ fs {fieldLuminary = FaceUp h}))
                    >> (return ())                
              _ -> return ()
      Children -> do
        currGS <- get
        case (gameDeck currGS) of
          a:b:c:rest -> do
            put $ currGS {gameDeck = rest, gameChildrenCards = [a,b,c]}
            defaultBehavior
          _ -> returnLeft "Not enough cards to reserve for the Children!"

resolveLuminaryTake :: Luminary -> PlayerIndex -> FailableGameAction ()
resolveLuminaryTake lum playerIndex = case lum of
  Rake -> do
    let 
      giveCardFromAToB card aInd bInd = do
        updateState $ liftPlayerS aInd (\ps -> return $ ps {playerHarvestPile = (playerHarvestPile ps) `setDelete` card})
        updateState $ liftPlayerS bInd (\ps -> return $ ps {playerHarvestPile = card : (playerHarvestPile ps)})
      filterForNonFoolSummer = filter $ \(Card val season) -> season == CSummer && val /= Fool
      filterForSummer = filter $ \(Card _ season) -> season == CSummer
      grabSummerCardFrom ind = do
        targetPlayer <- getPlayerS ind
        case filterForNonFoolSummer (playerHand targetPlayer) of
          [] -> case filterForSummer (playerHand targetPlayer) of
            [] -> return ()
            h:tl -> giveCardFromAToB h ind playerIndex
          h:tl -> giveCardFromAToB h ind playerIndex
    numPlayers <- withS $ (length . gamePlayerState)
    seqDo $ map grabSummerCardFrom (filter (/= playerIndex) (take numPlayers [0..]))
  Children -> do
    currChildrenCards <- withS gameChildrenCards
    updateState (\gs -> return $ gs {gameChildrenCards = []})
    updateState $ liftPlayerS playerIndex (\ps -> return $ ps {playerHarvestPile = currChildrenCards ++ (playerHarvestPile ps)})
  _ -> return ()


cardsCanHarvest :: [Card] -> [CardStack] -> Bool
cardsCanHarvest cardsUsed targetStacks = any (\handSum -> canHarvestWithNum handSum targetStacks) possibleHandSums
    where possibleHandSums = allPossibleSums [] cardsUsed
          allPossibleSums acc ((Card cardVal _):tl) = 
              let
                  acc' = (+) <$> (toNumberVals cardVal) <*> acc
              in
                  allPossibleSums acc' tl
          allPossibleSums acc [] = acc

canHarvestWithNum :: Int -> [CardStack] -> Bool
canHarvestWithNum num stacks = duplicateStacksMatchNum && additionsWork
    where duplicateStacksMatchNum = all (\(CardStack val cards) -> (length cards == 1) || num `elem` val) stacks
          additionsWork = any (\interp -> canFormPartition num interp) stackInterpretations
          stackInterpretations = foldr (\numVals accls -> (:) <$> numVals <*> accls) [[]] (map (\(CardStack val _) -> val) stacks)

allSubsets :: [a] -> [[a]]
allSubsets [] = [[]]
allSubsets (h:tl) = ((h:) <$> (allSubsets tl)) ++ (allSubsets tl)

allSubsetsSummingTo :: Int -> [Int] -> [[Int]]
allSubsetsSummingTo n ls = filter ((== n) . sum) (allSubsets ls)

setDelete :: (Eq a) => [a] -> a -> [a]
[] `setDelete` a = []
(x:xs) `setDelete` a
    | x == a = xs
    | otherwise = x : (xs `setDelete` a)

setSubtract :: (Eq a) => [a] -> [a] -> [a]
as `setSubtract` [] = as
as `setSubtract` (b:bs) = (as `setDelete` b) `setSubtract` bs

canFormPartition :: Int -> [Int] -> Bool
canFormPartition _ [] = True
canFormPartition partitionSum (h:tl)
    | h == partitionSum = canFormPartition partitionSum tl
    | h > partitionSum = False
    | otherwise =   let 
                        subsets = (allSubsetsSummingTo (partitionSum - h) tl)
                        trySubset subset = canFormPartition partitionSum (tl `setSubtract` subset)
                    in
                        any trySubset subsets

toNumberVals :: CardVal -> [Int]
toNumberVals Fool = [1, 14]
toNumberVals otherVal = [toNumberValWithFoolAsOne otherVal]         

toNumberValWithFoolAsOne :: CardVal -> Int
toNumberValWithFoolAsOne Fool = 1
toNumberValWithFoolAsOne otherVal = 1 + (toNumberValWithFoolAsOne $ pred otherVal)

toNumberValWithFoolAsFourteen :: CardVal -> Int
toNumberValWithFoolAsFourteen Two = 2
toNumberValWithFoolAsFourteen otherVal = 1 + (toNumberValWithFoolAsFourteen $ pred otherVal)

-- test

-- harvest :: PlayerIndex -> Card -> Direction -> [Card] -> GameState -> Maybe ((), GameState)
-- harvest playerIndex card fieldDirection targetCards gameState =
