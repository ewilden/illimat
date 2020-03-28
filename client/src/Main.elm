module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode exposing (encode)

import GameStateDecoder exposing (..)

fst = Tuple.first
snd = Tuple.second


---- MODEL ----

type alias Model =
  { gameStateModel: GameStateModel
  , selectedCardsModel: SelectedCardsModel
  }

type GameStateModel
  = Failure
  | Loading
  | Success GameState

type alias SelectedCardsModel = 
  { selectedHandCards: List Card
  }

combineInit : (m1 -> m2 -> m) -> (m1, Cmd Msg) -> (m2, Cmd Msg) -> (m, Cmd Msg)
combineInit f init1 init2 = 
  ( f (fst init1) (fst init2)
  , Cmd.batch [snd init1, snd init2]
  )

init : ( Model, Cmd Msg )
init = combineInit Model
        initGameStateModel 
        initSelectedCardsModel

initGameStateModel : (GameStateModel, Cmd Msg)
initGameStateModel = (Loading, getInitialGameState)

initSelectedCardsModel : (SelectedCardsModel, Cmd Msg)
initSelectedCardsModel = ({selectedHandCards = []}, Cmd.none)

getInitialGameState : Cmd Msg
getInitialGameState =
  Http.get
    { url = "http://localhost:3000/start"
    , expect = Http.expectJson GotGameState jsonDecGameState
    }



---- UPDATE ----


type Msg
  = MorePlease
  | GotGameState (Result Http.Error GameState)
  | SelectHandCard Card
  | UnselectHandCard Card

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    (gameStateModel, gameStateCmd) = updateGameStateModel msg model.gameStateModel
    (selectedCardsModel, selectedCardsCmd) = updateSelectedCardsModel msg model.selectedCardsModel
  in
    ( { gameStateModel = gameStateModel
      , selectedCardsModel = selectedCardsModel
      }
    , Cmd.batch [gameStateCmd, selectedCardsCmd] )

updateGameStateModel : Msg -> GameStateModel -> (GameStateModel, Cmd Msg)
updateGameStateModel msg model =
  case msg of
    MorePlease ->
      (Loading, getInitialGameState)

    GotGameState result ->
      case result of
        Ok gs ->
          (Success gs, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

    _ -> (model, Cmd.none)

updateSelectedCardsModel : Msg -> SelectedCardsModel -> (SelectedCardsModel, Cmd Msg)
updateSelectedCardsModel msg model = 
  case msg of
    SelectHandCard card -> ({model | selectedHandCards = card :: model.selectedHandCards}, Cmd.none)
    UnselectHandCard card -> ({model | selectedHandCards = List.filter (\c -> c /= card) model.selectedHandCards}, Cmd.none)
    _ -> (model, Cmd.none)



---- VIEW ----

view : Model -> Html Msg
view {gameStateModel, selectedCardsModel} =
  div []
    [ 
      case gameStateModel of 
        Success gameState -> viewGameState (gameState, selectedCardsModel)
        Failure -> text "Failure"
        Loading -> text "Not loaded yet"
    ]

nth : Int -> List a -> Maybe a
nth i ls =
  case List.drop i ls of
    [] -> Nothing
    h::_ -> Just h

viewPlayerFromIndex : ({ a | gamePlayerState: List PlayerState}, SelectedCardsModel) -> Int -> Html Msg
viewPlayerFromIndex (gameState, selectedCardsModel) playerIndex =
  nth playerIndex gameState.gamePlayerState
  |> Maybe.map (viewPlayerState selectedCardsModel)
  |> Maybe.withDefault (text "None")

viewPlayerState : SelectedCardsModel -> PlayerState -> Html Msg
viewPlayerState {selectedHandCards} playerState =
  div []
    [ p [] [text "Hand:"]
    , div 
        [ class "row"
        ]
        (List.map (\c -> viewCard (List.member c selectedHandCards) c) playerState.playerHand)
    ]

viewGameState : ( GameState
                , SelectedCardsModel ) -> Html Msg
viewGameState (gameState, selectedCardsModel) =
  div [ style "margin" "0 16px 0"]
    [
      p [] [text "Game board:"]
    , viewBoard (gameState, selectedCardsModel)
    -- , p [] [text "Everything:"]
    -- , text (encode 4 (jsonEncGameState <| fst (gameState, selectedCardsModel)))
    ]

grid : Int -> List (Html Msg) -> Html Msg
grid numColumns children =
  div
    (styleGrid numColumns)
    children

styleGrid : Int -> List (Attribute Msg)
styleGrid numColumns = 
  [ class "grid"
  , style "grid-template-columns" ("repeat(" ++ String.fromInt numColumns ++ ", 1fr)")
  ]

styleCell : List (Attribute Msg)
styleCell = [ class "cell" ]

styleCellAt : Int -> Int -> List (Attribute Msg)
styleCellAt row col =
  let
    span n = (String.fromInt n) ++ " / " ++ (String.fromInt (n + 1))
  in
    styleCell ++ 
    [ style "grid-row" (span row) 
    , style "grid-column" (span col)]

styleN : List (Attribute Msg)
styleN = styleCellAt 1 2

styleW : List (Attribute Msg)
styleW = styleCellAt 2 1

styleE : List (Attribute Msg)
styleE = styleCellAt 2 3

styleS : List (Attribute Msg)
styleS = styleCellAt 3 2

styleMid : List (Attribute Msg)
styleMid = styleCellAt 2 2

viewBoard : ({ a
              | gamePlayerState: List PlayerState
              , gameBoardState: BoardState
              , gameIllimatState: IllimatState }
            , SelectedCardsModel) -> Html Msg
viewBoard ( gameState , selectedCardsModel ) = 
  let
    {gamePlayerState, gameBoardState, gameIllimatState} = gameState
    showPlayer i =
      [ p [] [text ("Player " ++ String.fromInt i ++ ":")]
      , viewPlayerFromIndex (gameState, selectedCardsModel) (i - 1)
      ]
  in
    div [ style "margin" "0 16px 0" ]
      [ grid 3
          [ div
              (styleCellAt 1 1)
              (showPlayer 1)
          , div
              (styleCellAt 1 3)
              (showPlayer 3)
          , div
              (styleCellAt 3 1)
              (showPlayer 4)
          , div
              (styleCellAt 3 3)
              (showPlayer 2)
          , div 
              styleN
              [ p [] [text "North:"]
              , viewFieldState gameBoardState.bsFieldN
              ]
          , div 
              styleW
              [ p [] [text "West:"]
              , viewFieldState gameBoardState.bsFieldW
              ]
          , div 
              styleE
              [ p [] [text "East:"]
              , viewFieldState gameBoardState.bsFieldE
              ]
          , div 
              styleS
              [ p [] [text "South:"]
              , viewFieldState gameBoardState.bsFieldS
              ]
          , div
              (styleMid ++ 
                [ style "height" "100%"
                , style "flex-direction" "column"
                , style "display" "flex"])
              [ p [] [text "Illimat:"]
              , viewIllimatState gameIllimatState
              ]
          ]
      ]

viewIllimatState : IllimatState -> Html Msg
viewIllimatState illimatState =
  let
    summerDir = illimatState.illSummerDir
  in
    div [ class "IllimatContainer" ]
      [ div (styleGrid 3 ++ [ class "Illimat" ])
          [ div styleN
              [ dirToSeason summerDir N |> seasonToString |> text ]
          , div styleE
              [ dirToSeason summerDir E |> seasonToString |> text ]
          , div styleS
              [ dirToSeason summerDir S |> seasonToString |> text ]
          , div styleW
              [ dirToSeason summerDir W |> seasonToString |> text ]
          , div styleMid
              [ text ("Okuses: " ++ String.fromInt illimatState.illNumOkuses)]
          ]
      ]

dirToSeason : Direction -> Direction -> Season
dirToSeason summerDir dir =
  let
      dist = numRotatesFromAToB nextDirClockwise summerDir dir
  in
      rotateClockwise dist nextSeasonClockwise Summer

nextDirClockwise : Direction -> Direction
nextDirClockwise dir =
  case dir of
    N -> E
    E -> S
    S -> W
    W -> N

type Season =
  Summer
  | Spring
  | Winter
  | Autumn

nextSeasonClockwise : Season -> Season
nextSeasonClockwise season =
  case season of
    Summer -> Spring
    Spring -> Winter
    Winter -> Autumn
    Autumn -> Summer

numRotatesFromAToB : (a -> a) -> a -> a -> Int
numRotatesFromAToB rotator a b =
  if a == b then 0
  else 1 + numRotatesFromAToB rotator (rotator a) b

rotateClockwise : Int -> (a -> a) -> a -> a
rotateClockwise numRotates rotator a =
  if numRotates == 0 then a
  else rotateClockwise (numRotates - 1) rotator (rotator a)

viewFieldState : FieldState -> Html Msg
viewFieldState fieldState =
  div [ style "margin" "0 16px 0" 
      , style "flex" "1 1 100%"]
    [
      p [] [text "Cards:"]
    , div 
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-between"] 
        (List.map viewCardStack fieldState.fieldCards)
    , p [] [text "Luminary:"]
    , text <| luminaryStateToString fieldState.fieldLuminary
    ]

viewCardStack : CardStack -> Html Msg
viewCardStack (CardStack vals cards) =
  div [] 
  [
    case cards of
      [card] -> viewCard False card
      _ -> viewMultiCardStack (CardStack vals cards)
  ]

viewMultiCardStack : CardStack -> Html Msg
viewMultiCardStack (CardStack vals cards) =
  div
    [ style "padding" "2px"
    , style "border" "1px solid #888"
    , style "text-align" "center"
    , style "border-radius" "2px"]
    [ p [] [ text <| "Stack with possible values [" ++ (String.join "," <| List.map String.fromInt vals) ++ "]" ]
    , div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-around"
        ]
        (List.map (viewCard False) cards)
    ]

viewCard : Bool -> Card -> Html Msg
viewCard isSelected (Card val season) = 
  div 
    ([ style "padding" "4px"
      , style "border" "1px solid black"
      , style "text-align" "center"
      , style "border-radius" "4px"
      , (if isSelected then (style "border-color" "red") else (style "border-color" "black"))
      , onClick (if isSelected then 
            (UnselectHandCard (Card val season))
          else (SelectHandCard (Card val season))) ])
    [
      div [] [text (cardValToString val)]
    , div [] [text " of "]
    , div [] [text (cardSeasonToString season)]
    ]

cardValToString : CardVal -> String
cardValToString val = deQuote (encode 4 (jsonEncCardVal val))
  
deQuote : String -> String
deQuote = String.replace "\"" ""

deUnderscore : String -> String
deUnderscore = String.replace "_" " "

cardSeasonToString : CardSeason -> String
cardSeasonToString cardSeason =
  case cardSeason of
    CSummer -> "Summer"
    CSpring -> "Spring"
    CWinter -> "Winter"
    CAutumn -> "Autumn"
    CStars -> "Stars"

seasonToCardSeason : Season -> CardSeason
seasonToCardSeason season =
  case season of
    Summer -> CSummer
    Spring -> CSpring
    Winter -> CWinter
    Autumn -> CAutumn

seasonToString : Season -> String
seasonToString = cardSeasonToString << seasonToCardSeason

luminaryToString : Luminary -> String
luminaryToString lum = (deQuote << deUnderscore) (encode 4 (jsonEncLuminary lum))

luminaryStateToString : LuminaryState -> String
luminaryStateToString ls =
  case ls of
    FaceDown _ -> "<face-down Luminary>"
    FaceUp lum -> luminaryToString lum
    NoLuminary -> ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
