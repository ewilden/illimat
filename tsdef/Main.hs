module Main where

import           Prelude                        ( putStrLn )
import           RIO
import           Data.Proxy                     ( )
import           Data.Aeson.TypeScript.TH
import qualified Domain.Game                   as D
import qualified Domain.Game.GameLogic         as GL

main :: IO ()
main = putStrLn $ formatTSDeclarations
    (  (getTypeScriptDeclarations (Proxy :: Proxy GL.GameStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.IllimatState))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Direction))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.BoardStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.FieldStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.CardStack))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Card))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.CardVal))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.CardSeason))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Season))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.LuminaryStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Luminary))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.PlayerState))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.OtherPlayerView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.WhoseTurn))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.RakeTurn))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Move))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.FinishedGame))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.StartingGame))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.GameViewData))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.GameView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.CreateGameResponse))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.JoinGameResponse))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.StartGameResponse))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.JoinGameError))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.StartGameError))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.MakeMoveError))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.MakeMoveResponse))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.GetGameViewResponse))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.GetGameViewError))
    <> (getTypeScriptDeclarations (Proxy :: Proxy D.GetGamesForUserResponse))
    )
