module ElmConnect where

import ClassyPrelude
import GameLogic
import Data.Text.Conversions

import Elm.Derive
import Elm.Module

import Data.Proxy

$(deriveBoth defaultOptions ''GameState)
$(deriveBoth defaultOptions ''IllimatState)
$(deriveBoth defaultOptions ''Direction)
$(deriveBoth defaultOptions ''LuminaryState)
$(deriveBoth defaultOptions ''BoardState)
$(deriveBoth defaultOptions ''FieldState)
$(deriveBoth defaultOptions ''CardStack)
$(deriveBoth defaultOptions ''Card)
$(deriveBoth defaultOptions ''CardVal)
$(deriveBoth defaultOptions ''CardSeason)
$(deriveBoth defaultOptions ''PlayerState)
$(deriveBoth defaultOptions ''Luminary)