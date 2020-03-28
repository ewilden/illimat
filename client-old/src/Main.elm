-- Press a button to send a GET request for random cat GIFs.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode exposing (encode)

import GameStateDecoder exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { gameStateModel: GameStateModel
  , selectedCardsModel: SelectedCardsModel
  }

type GameStateModel
  = Failure
  | Loading
  | Success GameState

type alias SelectedCardsModel = 
  { selectedCards: List Card
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    (gsModel, gsCmd) = initGameStateModel ()
    (scModel, scCmd) = initSelectedCardsModel ()
  in
    (Model gsModel scModel, Cmd.batch [gsCmd, scCmd])

initGameStateModel : () -> (GameStateModel, Cmd Msg)
initGameStateModel _ = (Loading, getInitialGameState)

initSelectedCardsModel : () -> (SelectedCardsModel, Cmd Msg)
initSelectedCardsModel _ = ({selectedCards = []}, Cmd.none)


type Msg
  = MorePlease
  | GotGameState (Result Http.Error GameState)
  | SelectCard Card
  | UnselectCard Card


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    (gsModel, gsCmd) = updateGameStateModel msg model.gameStateModel
    (selectedCardsModel, scCmd) = updateSelectedCardsModel msg model.selectedCardsModel
  in
    ({gameStateModel = gsModel, selectedCardsModel = selectedCardsModel}, (Cmd.batch [gsCmd, scCmd]))

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
    SelectCard card -> ({model | selectedCards = card :: model.selectedCards}, Cmd.none)
    UnselectCard card -> ({model | selectedCards = List.filter (\c -> c /= card) model.selectedCards}, Cmd.none)
    _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view {gameStateModel, selectedCardsModel} =
  div []
    [ 
      case gameStateModel of 
        Success gameState -> viewGameState { gameState = gameState, selectedCardsModel = selectedCardsModel }
        Failure -> text "Failure"
        Loading -> text "Not loaded yet"
    ]

nth : Int -> List a -> Maybe a
nth i ls =
  case List.drop i ls of
    [] -> Nothing
    h::_ -> Just h

viewPlayerFromIndex : SelectedCardsModel -> { a | gamePlayerState: List PlayerState} -> Int -> Html Msg
viewPlayerFromIndex selectedCardsModel gameState playerIndex =
  nth playerIndex gameState.gamePlayerState
  |> Maybe.map (viewPlayerState selectedCardsModel)
  |> Maybe.withDefault (text "None")

viewPlayerState : SelectedCardsModel -> PlayerState -> Html Msg
viewPlayerState {selectedCards} playerState =
  div []
    [ p [] [text "Hand:"]
    , div 
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        (List.map (\c -> viewCard (List.member c selectedCards) c) playerState.playerHand)
    ]

viewGameState : { a | gameState: GameState
                    , selectedCardsModel: SelectedCardsModel } -> Html Msg
viewGameState { gameState, selectedCardsModel } =
  div [ style "margin" "0 16px 0"]
    [
      p [] [text "Game board:"]
    , viewBoard gameState selectedCardsModel
    , p [] [text "Everything:"]
    , text (encode 4 (jsonEncGameState gameState))
    ]

grid : Int -> List (Html Msg) -> Html Msg
grid numColumns children =
  div
    (styleGrid numColumns)
    children

styleGrid : Int -> List (Attribute Msg)
styleGrid numColumns = 
  [ style "display" "grid"
  , style "grid-template-columns" ("repeat(" ++ String.fromInt numColumns ++ ", 1fr)")
  , style "grid-gap" "30px"
  ]

styleCell : List (Attribute Msg)
styleCell = [ style "place-self" "center stretch"
            , style "flex" "1 1 100%"
            ]

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
  
viewBoard : { a 
            | gamePlayerState: List PlayerState
            , gameBoardState: BoardState
            , gameIllimatState: IllimatState } -> SelectedCardsModel -> Html Msg
viewBoard { gamePlayerState, gameBoardState, gameIllimatState } selectedCardsModel = 
  let
    showPlayer i =
      [ p [] [text ("Player " ++ String.fromInt i ++ ":")]
      , viewPlayerFromIndex selectedCardsModel { gamePlayerState = gamePlayerState } (i - 1)
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
    div [ style "flex" "1 1 100%"
        , style "border" "1px solid black"]
      [ div (styleGrid 3 ++ [ style "height" "100%"
                            , style "text-align" "center"])
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
            (UnselectCard (Card val season))
          else (SelectCard (Card val season))) ])
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

getInitialGameState : Cmd Msg
getInitialGameState =
  Http.get
    { url = "http://localhost:3000/start"
    , expect = Http.expectJson GotGameState jsonDecGameState
    }
