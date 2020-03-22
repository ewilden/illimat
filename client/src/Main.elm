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

type Model
  = Failure
  | Loading
  | Success GameState

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getInitialGameState)

type Msg
  = MorePlease
  | GotGameState (Result Http.Error GameState)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getInitialGameState)

    GotGameState result ->
      case result of
        Ok gs ->
          (Success gs, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  div []
    [ 
      case model of 
        Success gameState -> viewGameState gameState
        Failure -> text "Failure"
        Loading -> text "Not loaded yet"
    ]

viewGameState : GameState -> Html Msg
viewGameState gameState =
  div [ style "margin" "0 16px 0"]
    [
      p [] [text "BoardState:"]
    , viewBoardState gameState.gameBoardState
    , p [] [text "Everything:"]
    , text (encode 4 (jsonEncGameState gameState))
    ]
  
viewBoardState : BoardState -> Html Msg
viewBoardState boardState = 
  div [ style "margin" "0 16px 0" ]
    [ div [] 
        [ div []
            [ p [] [text "North:"]
            , viewFieldState boardState.bsFieldN
            ]
        , div [] 
            [ p [] [text "South:"]
            , viewFieldState boardState.bsFieldS
            ]
        , div [] 
            [ p [] [text "West:"]
            , viewFieldState boardState.bsFieldW
            ]
        , div [] 
            [ p [] [text "East:"]
            , viewFieldState boardState.bsFieldE
            ]
        ]
    ]

viewFieldState : FieldState -> Html Msg
viewFieldState fieldState =
  div [ style "margin" "0 16px 0" ]
    [
      p [] [text "Cards:"]
    , div [] (List.map viewCardStack fieldState.fieldCards)
    , p [] [text "Luminary:"]
    , text (encode 4 (jsonEncLuminaryState fieldState.fieldLuminary))
    ]

viewCardStack : CardStack -> Html Msg
viewCardStack (CardStack vals cards) =
  div [style "display" "inline-block"] 
  [
    case cards of
      [card] -> viewCard card
      _ -> text (encode 4 (jsonEncCardStack (CardStack vals cards)))
  ]

viewCard : Card -> Html Msg
viewCard (Card val season) = 
  div [ style "padding" "4px"
      , style "margin" "8px"
      , style "border" "1px solid black"
      , style "text-align" "center"
      , style "border-radius" "4px"] [
    div [] [text (cardValToString val)]
  , div [] [text " of "]
  , div [] [text (cardSeasonToString season)]
  ]

cardValToString : CardVal -> String
cardValToString val = 
  case val of 
    Fool -> "Fool"
    Two -> "Two"
    Three -> "Three"
    Four -> "Four"
    Five -> "Five"
    Six -> "Six"
    Seven -> "Seven"
    Eight -> "Eight"
    Nine -> "Nine"
    Ten -> "Ten"
    Knight -> "Knight"
    Queen -> "Queen"
    King -> "King"

cardSeasonToString : CardSeason -> String
cardSeasonToString cardSeason =
  case cardSeason of
    CSummer -> "Summer"
    CSpring -> "Spring"
    CWinter -> "Winter"
    CAutumn -> "Autumn"
    CStars -> "Stars"

getInitialGameState : Cmd Msg
getInitialGameState =
  Http.get
    { url = "http://localhost:3000/start"
    , expect = Http.expectJson GotGameState jsonDecGameState
    }
