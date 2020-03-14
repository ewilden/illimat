module GameStateDecoder exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias GameState  =
   { gameIllimatState: IllimatState
   , gameBoardState: BoardState
   , gamePlayerState: (List PlayerState)
   , gameDeck: (List Card)
   , gameUnusedLuminaries: (List Luminary)
   , gameChildrenCards: (List Card)
   }

jsonDecGameState : Json.Decode.Decoder ( GameState )
jsonDecGameState =
   Json.Decode.succeed (\pgameIllimatState pgameBoardState pgamePlayerState pgameDeck pgameUnusedLuminaries pgameChildrenCards -> {gameIllimatState = pgameIllimatState, gameBoardState = pgameBoardState, gamePlayerState = pgamePlayerState, gameDeck = pgameDeck, gameUnusedLuminaries = pgameUnusedLuminaries, gameChildrenCards = pgameChildrenCards})
   |> required "gameIllimatState" (jsonDecIllimatState)
   |> required "gameBoardState" (jsonDecBoardState)
   |> required "gamePlayerState" (Json.Decode.list (jsonDecPlayerState))
   |> required "gameDeck" (Json.Decode.list (jsonDecCard))
   |> required "gameUnusedLuminaries" (Json.Decode.list (jsonDecLuminary))
   |> required "gameChildrenCards" (Json.Decode.list (jsonDecCard))

jsonEncGameState : GameState -> Value
jsonEncGameState  val =
   Json.Encode.object
   [ ("gameIllimatState", jsonEncIllimatState val.gameIllimatState)
   , ("gameBoardState", jsonEncBoardState val.gameBoardState)
   , ("gamePlayerState", (Json.Encode.list jsonEncPlayerState) val.gamePlayerState)
   , ("gameDeck", (Json.Encode.list jsonEncCard) val.gameDeck)
   , ("gameUnusedLuminaries", (Json.Encode.list jsonEncLuminary) val.gameUnusedLuminaries)
   , ("gameChildrenCards", (Json.Encode.list jsonEncCard) val.gameChildrenCards)
   ]



type alias IllimatState  =
   { illSummerDir: Direction
   , illNumOkuses: Int
   }

jsonDecIllimatState : Json.Decode.Decoder ( IllimatState )
jsonDecIllimatState =
   Json.Decode.succeed (\pillSummerDir pillNumOkuses -> {illSummerDir = pillSummerDir, illNumOkuses = pillNumOkuses})
   |> required "illSummerDir" (jsonDecDirection)
   |> required "illNumOkuses" (Json.Decode.int)

jsonEncIllimatState : IllimatState -> Value
jsonEncIllimatState  val =
   Json.Encode.object
   [ ("illSummerDir", jsonEncDirection val.illSummerDir)
   , ("illNumOkuses", Json.Encode.int val.illNumOkuses)
   ]



type Direction  =
    N 
    | E 
    | S 
    | W 

jsonDecDirection : Json.Decode.Decoder ( Direction )
jsonDecDirection = 
    let jsonDecDictDirection = Dict.fromList [("N", N), ("E", E), ("S", S), ("W", W)]
    in  decodeSumUnaries "Direction" jsonDecDictDirection

jsonEncDirection : Direction -> Value
jsonEncDirection  val =
    case val of
        N -> Json.Encode.string "N"
        E -> Json.Encode.string "E"
        S -> Json.Encode.string "S"
        W -> Json.Encode.string "W"



type LuminaryState  =
    FaceUp Luminary
    | FaceDown Luminary
    | NoLuminary 

jsonDecLuminaryState : Json.Decode.Decoder ( LuminaryState )
jsonDecLuminaryState =
    let jsonDecDictLuminaryState = Dict.fromList
            [ ("FaceUp", Json.Decode.lazy (\_ -> Json.Decode.map FaceUp (jsonDecLuminary)))
            , ("FaceDown", Json.Decode.lazy (\_ -> Json.Decode.map FaceDown (jsonDecLuminary)))
            , ("NoLuminary", Json.Decode.lazy (\_ -> Json.Decode.succeed NoLuminary))
            ]
    in  decodeSumObjectWithSingleField  "LuminaryState" jsonDecDictLuminaryState

jsonEncLuminaryState : LuminaryState -> Value
jsonEncLuminaryState  val =
    let keyval v = case v of
                    FaceUp v1 -> ("FaceUp", encodeValue (jsonEncLuminary v1))
                    FaceDown v1 -> ("FaceDown", encodeValue (jsonEncLuminary v1))
                    NoLuminary  -> ("NoLuminary", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type alias BoardState  =
   { bsFieldN: FieldState
   , bsFieldE: FieldState
   , bsFieldS: FieldState
   , bsFieldW: FieldState
   }

jsonDecBoardState : Json.Decode.Decoder ( BoardState )
jsonDecBoardState =
   Json.Decode.succeed (\pbsFieldN pbsFieldE pbsFieldS pbsFieldW -> {bsFieldN = pbsFieldN, bsFieldE = pbsFieldE, bsFieldS = pbsFieldS, bsFieldW = pbsFieldW})
   |> required "bsFieldN" (jsonDecFieldState)
   |> required "bsFieldE" (jsonDecFieldState)
   |> required "bsFieldS" (jsonDecFieldState)
   |> required "bsFieldW" (jsonDecFieldState)

jsonEncBoardState : BoardState -> Value
jsonEncBoardState  val =
   Json.Encode.object
   [ ("bsFieldN", jsonEncFieldState val.bsFieldN)
   , ("bsFieldE", jsonEncFieldState val.bsFieldE)
   , ("bsFieldS", jsonEncFieldState val.bsFieldS)
   , ("bsFieldW", jsonEncFieldState val.bsFieldW)
   ]



type alias FieldState  =
   { fieldCards: (List CardStack)
   , fieldLuminary: LuminaryState
   }

jsonDecFieldState : Json.Decode.Decoder ( FieldState )
jsonDecFieldState =
   Json.Decode.succeed (\pfieldCards pfieldLuminary -> {fieldCards = pfieldCards, fieldLuminary = pfieldLuminary})
   |> required "fieldCards" (Json.Decode.list (jsonDecCardStack))
   |> required "fieldLuminary" (jsonDecLuminaryState)

jsonEncFieldState : FieldState -> Value
jsonEncFieldState  val =
   Json.Encode.object
   [ ("fieldCards", (Json.Encode.list jsonEncCardStack) val.fieldCards)
   , ("fieldLuminary", jsonEncLuminaryState val.fieldLuminary)
   ]



type CardStack  =
    CardStack (List Int) (List Card)

jsonDecCardStack : Json.Decode.Decoder ( CardStack )
jsonDecCardStack =
    Json.Decode.lazy (\_ -> Json.Decode.map2 CardStack (Json.Decode.index 0 (Json.Decode.list (Json.Decode.int))) (Json.Decode.index 1 (Json.Decode.list (jsonDecCard))))


jsonEncCardStack : CardStack -> Value
jsonEncCardStack (CardStack v1 v2) =
    Json.Encode.list identity [(Json.Encode.list Json.Encode.int) v1, (Json.Encode.list jsonEncCard) v2]



type Card  =
    Card CardVal CardSeason

jsonDecCard : Json.Decode.Decoder ( Card )
jsonDecCard =
    Json.Decode.lazy (\_ -> Json.Decode.map2 Card (Json.Decode.index 0 (jsonDecCardVal)) (Json.Decode.index 1 (jsonDecCardSeason)))


jsonEncCard : Card -> Value
jsonEncCard (Card v1 v2) =
    Json.Encode.list identity [jsonEncCardVal v1, jsonEncCardSeason v2]



type CardVal  =
    Fool 
    | Two 
    | Three 
    | Four 
    | Five 
    | Six 
    | Seven 
    | Eight 
    | Nine 
    | Ten 
    | Knight 
    | Queen 
    | King 

jsonDecCardVal : Json.Decode.Decoder ( CardVal )
jsonDecCardVal = 
    let jsonDecDictCardVal = Dict.fromList [("Fool", Fool), ("Two", Two), ("Three", Three), ("Four", Four), ("Five", Five), ("Six", Six), ("Seven", Seven), ("Eight", Eight), ("Nine", Nine), ("Ten", Ten), ("Knight", Knight), ("Queen", Queen), ("King", King)]
    in  decodeSumUnaries "CardVal" jsonDecDictCardVal

jsonEncCardVal : CardVal -> Value
jsonEncCardVal  val =
    case val of
        Fool -> Json.Encode.string "Fool"
        Two -> Json.Encode.string "Two"
        Three -> Json.Encode.string "Three"
        Four -> Json.Encode.string "Four"
        Five -> Json.Encode.string "Five"
        Six -> Json.Encode.string "Six"
        Seven -> Json.Encode.string "Seven"
        Eight -> Json.Encode.string "Eight"
        Nine -> Json.Encode.string "Nine"
        Ten -> Json.Encode.string "Ten"
        Knight -> Json.Encode.string "Knight"
        Queen -> Json.Encode.string "Queen"
        King -> Json.Encode.string "King"



type CardSeason  =
    CSummer 
    | CSpring 
    | CWinter 
    | CAutumn 
    | CStars 

jsonDecCardSeason : Json.Decode.Decoder ( CardSeason )
jsonDecCardSeason = 
    let jsonDecDictCardSeason = Dict.fromList [("CSummer", CSummer), ("CSpring", CSpring), ("CWinter", CWinter), ("CAutumn", CAutumn), ("CStars", CStars)]
    in  decodeSumUnaries "CardSeason" jsonDecDictCardSeason

jsonEncCardSeason : CardSeason -> Value
jsonEncCardSeason  val =
    case val of
        CSummer -> Json.Encode.string "CSummer"
        CSpring -> Json.Encode.string "CSpring"
        CWinter -> Json.Encode.string "CWinter"
        CAutumn -> Json.Encode.string "CAutumn"
        CStars -> Json.Encode.string "CStars"



type alias PlayerState  =
   { playerHand: (List Card)
   , playerNumOkuses: Int
   , playerLuminaries: (List Luminary)
   , playerHarvestPile: (List Card)
   }

jsonDecPlayerState : Json.Decode.Decoder ( PlayerState )
jsonDecPlayerState =
   Json.Decode.succeed (\pplayerHand pplayerNumOkuses pplayerLuminaries pplayerHarvestPile -> {playerHand = pplayerHand, playerNumOkuses = pplayerNumOkuses, playerLuminaries = pplayerLuminaries, playerHarvestPile = pplayerHarvestPile})
   |> required "playerHand" (Json.Decode.list (jsonDecCard))
   |> required "playerNumOkuses" (Json.Decode.int)
   |> required "playerLuminaries" (Json.Decode.list (jsonDecLuminary))
   |> required "playerHarvestPile" (Json.Decode.list (jsonDecCard))

jsonEncPlayerState : PlayerState -> Value
jsonEncPlayerState  val =
   Json.Encode.object
   [ ("playerHand", (Json.Encode.list jsonEncCard) val.playerHand)
   , ("playerNumOkuses", Json.Encode.int val.playerNumOkuses)
   , ("playerLuminaries", (Json.Encode.list jsonEncLuminary) val.playerLuminaries)
   , ("playerHarvestPile", (Json.Encode.list jsonEncCard) val.playerHarvestPile)
   ]



type Luminary  =
    Union 
    | Maiden 
    | Rake 
    | River 
    | Changeling 
    | Newborn 
    | Forest_Queen 
    | Children 

jsonDecLuminary : Json.Decode.Decoder ( Luminary )
jsonDecLuminary = 
    let jsonDecDictLuminary = Dict.fromList [("Union", Union), ("Maiden", Maiden), ("Rake", Rake), ("River", River), ("Changeling", Changeling), ("Newborn", Newborn), ("Forest_Queen", Forest_Queen), ("Children", Children)]
    in  decodeSumUnaries "Luminary" jsonDecDictLuminary

jsonEncLuminary : Luminary -> Value
jsonEncLuminary  val =
    case val of
        Union -> Json.Encode.string "Union"
        Maiden -> Json.Encode.string "Maiden"
        Rake -> Json.Encode.string "Rake"
        River -> Json.Encode.string "River"
        Changeling -> Json.Encode.string "Changeling"
        Newborn -> Json.Encode.string "Newborn"
        Forest_Queen -> Json.Encode.string "Forest_Queen"
        Children -> Json.Encode.string "Children"


