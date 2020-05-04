{-# LANGUAGE TemplateHaskell #-}

module Domain.Game.GameLogic where

-- import           Prelude                        ( head )
import RIO
import qualified Prelude ((!!))
import Prelude (putStrLn, splitAt)
import qualified RIO.Partial as Partial
import           RIO.State
import Data.Aeson.TH ()
import Data.Aeson.TypeScript.TH
import           Data.Sort

import qualified AesonOptions

-- loops around back to minBound
succ :: (Bounded a, Eq a, Enum a) => a -> a
succ a = if a == maxBound then minBound else Partial.succ a

pred :: (Bounded a, Eq a, Enum a) => a -> a
pred a = if a == minBound then maxBound else Partial.pred a

headOr :: a -> [a] -> a
headOr _ (h : _) = h
headOr a []       = a

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

(!!) :: [a] -> Int -> Maybe a
[]       !! _ = Nothing
(h : _) !! 0 = Just h
(_ : tl) !! n | n < 0     = Nothing
              | otherwise = tl !! (n - 1)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GameState = GameState
    { _gameIllimatState :: !IllimatState
    , _gameBoardState :: !BoardState
    , _gamePlayerState :: ![PlayerState]
    , _gameDeck :: ![Card]
    , _gameUnusedLuminaries :: ![Luminary]
    , _gameChildrenCards :: ![Card]
    , _gameWhoseTurn :: !WhoseTurn
    } deriving (Show)

data GameStateView = GameStateView
  { _viewIllimatState :: !IllimatState
  , _viewBoardState :: !BoardStateView
  , _viewPlayerState :: !(PlayerState, [OtherPlayerView])
  , _viewDeck :: !Int -- num cards in deck
  -- nothing for unused luminaries
  -- nothing for children cards (whenever the Children is face up, it has cards)
  , _viewWhoseTurn :: !WhoseTurn
  } deriving (Show, Eq)

data WhoseTurn = WhoseTurn PlayerIndex RakeTurn
  deriving (Show, Eq)

data RakeTurn = NoRake | RakeBoth | RakeSow | RakeOtherMove
  deriving (Show, Eq)

computeViewForPlayer :: PlayerIndex -> GameState -> GameStateView
computeViewForPlayer playerIndex (GameState ill board players deck unusedLums childrenCards whoseTurn) =
  GameStateView
    { _viewIllimatState = ill
    , _viewBoardState = computeBoardView board
    , _viewPlayerState = ( players Prelude.!! playerIndex
                         , players & (map computeOtherPlayerView)
                                   . (map snd)
                                   . (filter (\(i, _) -> i /= playerIndex))
                                   . (zip [0..])
                         )
    , _viewDeck = length deck
    , _viewWhoseTurn = whoseTurn
    }


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
    { _playerHand :: ![Card]
    , _playerNumOkuses :: !Int
    , _playerLuminaries :: ![Luminary]
    , _playerHarvestPile :: ![Card]
    } deriving (Show, Eq)

data OtherPlayerView = OtherPlayerView
  { _opNumCardsInHand :: !Int
  , _opNumOkuses :: !Int
  , _opLuminaries :: ![Luminary]
  , _opHarvestPileSize :: !Int
      -- how many face down cards to show. Consider bucketing to reduce info
  } deriving (Show, Eq)

computeOtherPlayerView :: PlayerState -> OtherPlayerView
computeOtherPlayerView (PlayerState hand numOkuses lums harvestPile) =
  OtherPlayerView
    { _opNumCardsInHand = length hand
    , _opNumOkuses = numOkuses
    , _opLuminaries = lums
    , _opHarvestPileSize = (length harvestPile) `div` 4
    }

data IllimatState = IllimatState
    { _illSummerDir :: !Direction
    , _illNumOkuses :: !Int
    } deriving (Show, Eq)

data BoardState = BoardState
    { _bsFieldN :: !FieldState
    , _bsFieldE :: !FieldState
    , _bsFieldS :: !FieldState
    , _bsFieldW :: !FieldState
    } deriving (Show)

data BoardStateView = BoardStateView
    { _viewFieldN :: !FieldStateView
    , _viewFieldE :: !FieldStateView
    , _viewFieldS :: !FieldStateView
    , _viewFieldW :: !FieldStateView
    } deriving (Show, Eq)

computeBoardView :: BoardState -> BoardStateView
computeBoardView (BoardState n e s w) =
  BoardStateView
    (computeFieldView n)
    (computeFieldView e)
    (computeFieldView s)
    (computeFieldView w)

data FieldState = FieldState
    { _fieldCards :: ![CardStack]
    , _fieldLuminary :: !LuminaryState
    } deriving (Show)

data FieldStateView = FieldStateView
  { _vfCards :: ![CardStack]
  , _vfLuminary :: !LuminaryStateView
  } deriving (Show, Eq)

computeFieldView :: FieldState -> FieldStateView
computeFieldView (FieldState cards lum) =
  FieldStateView
    cards
    (case lum of
      NoLuminary -> NoLuminaryView
      FaceDown _ -> FaceDownView
      FaceUp l -> FaceUpView l
    )

data LuminaryState = FaceUp Luminary | FaceDown Luminary | NoLuminary deriving (Show, Eq)
data Luminary = Union | Maiden | Rake | River | Changeling | Newborn | Forest_Queen | Children
    deriving (Show, Eq, Enum, Bounded)
data LuminaryStateView = FaceUpView Luminary | FaceDownView | NoLuminaryView deriving (Show, Eq)

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

indexFromN :: Direction -> Int
indexFromN N = 0
indexFromN E = 1
indexFromN S = 2
indexFromN W = 3

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
            , _gameWhoseTurn = WhoseTurn (indexFromN initSummerDir) NoRake
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
  (pre_, x : post) -> Just $ pre_ ++ (f x : post)
  _               -> Nothing

-- <&> = flip <$>

patchListEith :: s -> Int -> (a -> Either s a) -> [a] -> Either s [a]
patchListEith failMsg ind f ls = case splitAt ind ls of
  (pre_, x : post) -> f x <&> (: post) <&> (pre_ ++)
  _               -> Left failMsg

type FailableGameAction a = StateT GameState (Either Text) a

runGS :: FailableGameAction a -> GameState -> Either Text (a, GameState)
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

returnLeft :: Text -> FailableGameAction a
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

updateState :: (GameState -> Either Text GameState) -> FailableGameAction ()
updateState f = do
  s <- get
  case f s of
    Left  errMsg -> returnLeft errMsg
    Right gs     -> put gs

mapPlayerM
  :: PlayerIndex
  -> (PlayerState -> Either Text PlayerState)
  -> (GameState -> Either Text GameState)
mapPlayerM playerInd f gs =
  let prevPlayers = _gamePlayerState gs
      patchResult = patchListEith "no such player" playerInd f prevPlayers
  in  (\nextPlayers -> gs { _gamePlayerState = nextPlayers }) <$> patchResult

checkS :: Bool -> Text -> FailableGameAction ()
checkS cond failureMsg = if cond then return () else returnLeft failureMsg

checkNotS :: Bool -> Text -> FailableGameAction ()
checkNotS failCond failureMsg =
  if failCond then returnLeft failureMsg else return ()

doNTimes :: Int -> FailableGameAction a -> FailableGameAction ()
doNTimes n action = sequence_ $ map (const action) $ (replicate n () :: [()])

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
  checkS playerHasCards' "Player doesn't have those cards"
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
  sequence_ (map dealLuminaryToField (allEnum :: [Direction]))
  sequence_ (map deal3CardsToField (allEnum :: [Direction]))
  currPlayers <- withS _gamePlayerState
  (WhoseTurn firstTurnTaker _) <- withS _gameWhoseTurn
  fillPlayersHandToN 3 firstTurnTaker
  sequence_ (map (\(_, i) -> fillPlayersHandToN 4 i) $ filter (\(_, i) -> i /= firstTurnTaker) $ zip currPlayers [0 ..])

fillPlayersHandToN :: Int -> PlayerIndex -> FailableGameAction ()
fillPlayersHandToN numCards playerIndex = do
  currPlayer <- getPlayerS playerIndex
  doNTimes (numCards - (length $ _playerHand currPlayer))
           (doAllowingFailure $ dealCardToPlayer playerIndex)

updateWhoseTurn :: Bool -> FailableGameAction ()
updateWhoseTurn wasRakeSow = do
  numPlayers <- withS $ length . _gamePlayerState
  let succpi i = (i + 1) `mod` numPlayers
  (WhoseTurn turnOwner rakeTurn) <- withS _gameWhoseTurn
  rakeIsOnBoard <- withS $
    any (\fs -> _fieldLuminary fs == FaceUp Rake)
    . (\gs -> map (flip fieldFromDir gs) (allEnum :: [Direction]))
  case (rakeIsOnBoard, rakeTurn) of
    (False, _) -> updateState (\gs -> return $ gs { _gameWhoseTurn = WhoseTurn (succpi turnOwner) NoRake})
    (True, RakeOtherMove) -> updateState (\gs -> return $ gs { _gameWhoseTurn = WhoseTurn (succpi turnOwner) RakeBoth})
    (True, RakeSow) -> updateState (\gs -> return $ gs { _gameWhoseTurn = WhoseTurn (succpi turnOwner) RakeBoth})
    (True, _) -> updateState (\gs -> return $ gs {_gameWhoseTurn = WhoseTurn turnOwner (if wasRakeSow then RakeOtherMove else RakeSow)})

stockpile
  :: PlayerIndex
  -> Card
  -> Direction
  -> [CardStack]
  -> Int
  -> FailableGameAction ()
stockpile playerIndex card dir targetStacks desiredStackVal = do
  (WhoseTurn turnOwner rakeTurn) <- withS _gameWhoseTurn
  case (playerIndex == turnOwner, rakeTurn) of
    (False, _) -> returnLeft "It's not this player's turn!"
    (True, RakeSow) -> returnLeft "You must sow for the Rake."
    (True, _) -> return () -- pass
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
  sequence_ $ map (\stack -> removeCardStackFromField stack dir) targetStacks
  updateState $ mapFieldM
    dir
    (\fs -> return $ fs { _fieldCards = resultingStack : (_fieldCards fs) })
  resolveSeasonChange card dir
  updateWhoseTurn False

sow :: PlayerIndex -> Card -> Direction -> FailableGameAction ()
sow playerIndex card dir = do
  fieldHasFaceUpRake <- withS $ (== FaceUp Rake) . _fieldLuminary . fieldFromDir dir
  currPlayer <- getPlayerS playerIndex
  checkS (card `elem` _playerHand currPlayer)
         "Player doesn't have the card they tried to sow!"
  (_, season) <- withS $ fieldSFromDir dir
  checkNotS (season == Autumn && not fieldHasFaceUpRake)
            "Can't sow because it's autumn!"
  removeCardsFromPlayersHand playerIndex [card]
  updateState $ mapFieldM
    dir
    (\fs -> return fs { _fieldCards = (fromCard card) : (_fieldCards fs) })
  resolveSeasonChange card dir
  updateWhoseTurn fieldHasFaceUpRake

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
  checkNotS (currSeason == Winter && not maidenIsActive)
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
  sequence_ (map (\stack -> removeCardStackFromField stack fieldDir) targetStacks)
  addCardStacksToHarvestPile playerIndex targetStacks
  addCardStacksToHarvestPile playerIndex (map fromCard playedCards)

  sequence_ $ map (\card -> resolveSeasonChange card fieldDir) playedCards

  -- re-fill player's hand up to 4
  resultingPlayer <- getPlayerS playerIndex
  doNTimes (4 - length (_playerHand resultingPlayer))
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
  updateWhoseTurn False

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
    sequence_ $ map grabSummerCardFrom
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

isGameOver :: GameState -> Bool
isGameOver gameState = all (== []) $ map _playerHand $ _gamePlayerState gameState

-- scoring
score :: [PlayerState] -> [Int]
score players = flip execState (map (const 0) players) $ do
  let
    givePointsTo :: Int -> PlayerIndex -> State [Int] ()
    givePointsTo numPoints playerIndex = do
      currPoints <- get
      put $ currPoints
        & zip [0..]
        & map (\x@(ind, pts) -> if ind == playerIndex then (ind, pts + numPoints) else x)
        & map snd
    indicesOfHighest :: [(PlayerIndex, Int)] -> [PlayerIndex]
    indicesOfHighest scores = scores
      & groupSortBy cmp (:)
      & headOr []
      & map fst
        where cmp (ind1, score1) (ind2, score2) = compare score2 score1
    luminaryCounts :: [(PlayerIndex, Int)]
    luminaryCounts = players
      & map (length . _playerLuminaries)
      & zip [0..]
    cardCounts :: [(PlayerIndex, Int)]
    cardCounts = players
      & map (length . _playerHarvestPile)
      & zip [0..]
    summerCounts :: [(PlayerIndex, Int)]
    summerCounts = players
      & map _playerHarvestPile
      & map (filter (\(Card _ season) -> season == CSummer))
      & map length
      & zip [0..]
    winterCounts :: [(PlayerIndex, Int)]
    winterCounts = players
      & map _playerHarvestPile
      & map (filter (\(Card _ season) -> season == CWinter))
      & map length
      & zip [0..]
    foolCounts :: [(PlayerIndex, Int)]
    foolCounts = players
      & map _playerHarvestPile
      & map (filter (\(Card val _) -> val == Fool))
      & map length
      & zip [0..]
    okusCounts :: [(PlayerIndex, Int)]
    okusCounts = players
      & map _playerNumOkuses
      & zip [0..]
    givePointForEach :: [(PlayerIndex, Int)] -> State [Int] ()
    givePointForEach counts = counts
      & map (\(ind, num) -> givePointsTo num ind)
      & sequence_
    forceLookup :: PlayerIndex -> [(PlayerIndex, a)] -> a
    forceLookup playerIndex playerData = case lookup playerIndex playerData of
      Nothing -> error "impossible"
      Just a -> a
  givePointForEach luminaryCounts
  givePointForEach okusCounts
  givePointForEach foolCounts
  let
    mostCardPlayers = indicesOfHighest cardCounts
    mostSummerPlayers = indicesOfHighest summerCounts
    mostWinterPlayers = indicesOfHighest winterCounts
    tiebreakByLuminaries :: Bool -> [PlayerIndex] -> Maybe PlayerIndex
    tiebreakByLuminaries byFewest playerIndices = case playerIndices of
      [singleWinner] -> Just singleWinner
      multipleWinners -> do
        let lumTiebreakCounts = playerIndices
              & map (\i -> (i, forceLookup i luminaryCounts))
              & map (\(i, count) -> if byFewest then (i, -count) else (i, count))
        case indicesOfHighest lumTiebreakCounts of
          [finalWinner] -> Just finalWinner
          _ -> Nothing
  -- award points for most cards
  case (tiebreakByLuminaries False mostCardPlayers) of
    Just player -> givePointsTo 4 player
    Nothing -> return ()
  case (tiebreakByLuminaries False mostSummerPlayers) of
    Just player -> givePointsTo 2 player
    Nothing -> return ()
  case (tiebreakByLuminaries True mostWinterPlayers) of
    Just playerIndex ->
      if
        any (== River) $ _playerLuminaries $ forceLookup playerIndex $ zip [0..] players
      then
        givePointsTo 2 playerIndex
      else
        givePointsTo (-2) playerIndex
    Nothing -> return ()

data Move
  = Harvest [Card] Direction [CardStack]
  | Sow Card Direction
  | Stockpile Card Direction [CardStack] Int
  deriving (Show, Eq)

executeMove :: PlayerIndex -> Move -> FailableGameAction ()
executeMove playerIndex move = case move of
  Harvest cards dir stacks -> harvest playerIndex cards dir stacks
  Sow card dir -> sow playerIndex card dir
  Stockpile card dir stacks desiredStackVal -> stockpile playerIndex card dir stacks desiredStackVal

$(deriveJSONAndTypeScript AesonOptions.options ''IllimatState)
$(deriveJSONAndTypeScript AesonOptions.options ''Direction)
$(deriveJSONAndTypeScript AesonOptions.options ''BoardStateView)
$(deriveJSONAndTypeScript AesonOptions.options ''FieldStateView)
$(deriveJSONAndTypeScript AesonOptions.options ''CardStack)
$(deriveJSONAndTypeScript AesonOptions.options ''Card)
$(deriveJSONAndTypeScript AesonOptions.options ''CardVal)
$(deriveJSONAndTypeScript AesonOptions.options ''CardSeason)
$(deriveJSONAndTypeScript AesonOptions.options ''Season)
$(deriveJSONAndTypeScript AesonOptions.options ''LuminaryStateView)
$(deriveJSONAndTypeScript AesonOptions.options ''Luminary)
$(deriveJSONAndTypeScript AesonOptions.options ''PlayerState)
$(deriveJSONAndTypeScript AesonOptions.options ''GameStateView)
$(deriveJSONAndTypeScript AesonOptions.options ''OtherPlayerView)
$(deriveJSONAndTypeScript AesonOptions.options ''WhoseTurn)
$(deriveJSONAndTypeScript AesonOptions.options ''RakeTurn)