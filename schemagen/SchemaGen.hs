module SchemaGen where

import ClassyPrelude
import GameLogic
import Data.Text.Conversions

import Elm.Derive
import Elm.Module

import Data.Proxy
import ElmConnect

main :: IO ()
main =
    putStrLn $ toText $ makeElmModule "GameStateDecoder"
    [ DefineElm (Proxy :: Proxy GameState)        
    , DefineElm (Proxy :: Proxy IllimatState)
    , DefineElm (Proxy :: Proxy Direction)
    , DefineElm (Proxy :: Proxy LuminaryState)
    , DefineElm (Proxy :: Proxy BoardState)
    , DefineElm (Proxy :: Proxy FieldState)
    , DefineElm (Proxy :: Proxy CardStack)
    , DefineElm (Proxy :: Proxy Card)
    , DefineElm (Proxy :: Proxy CardVal)
    , DefineElm (Proxy :: Proxy CardSeason)
    , DefineElm (Proxy :: Proxy PlayerState)
    , DefineElm (Proxy :: Proxy Luminary)
    ]