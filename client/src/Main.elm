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



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success GameState


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getInitialGameState)



-- UPDATE


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
      viewGS model
    ]

viewGS : Model -> Html Msg
viewGS model =
  case model of
    Success gs ->
      text (encode 4 (jsonEncGameState gs))
    Failure -> text "Failure"
    Loading -> text "Not loaded yet"

-- viewGif : Model -> Html Msg
-- viewGif model =
--   case model of
--     Failure ->
--       div []
--         [ text "I could not load a random cat for some reason. "
--         , button [ onClick MorePlease ] [ text "Try Again!" ]
--         ]

--     Loading ->
--       text "Loading..."

--     Success url ->
--       div []
--         [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
--         , img [ src url ] []
--         ]



-- HTTP


getInitialGameState : Cmd Msg
getInitialGameState =
  Http.get
    { url = "http://localhost:3000/start"
    , expect = Http.expectJson GotGameState jsonDecGameState
    }


-- gifDecoder : Decoder String
-- gifDecoder =
--   field "data" (field "image_url" string)