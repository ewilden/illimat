module Main where

import Prelude (putStrLn)
import RIO
import Data.Proxy ()
import Data.Aeson.TypeScript.TH
import qualified Domain.Game.GameLogic as GL

main :: IO ()
main = putStrLn $ formatTSDeclarations (
    (getTypeScriptDeclarations (Proxy :: Proxy GL.GameStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.IllimatState))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Direction))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.BoardStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.FieldStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.CardStack))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Card))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.CardVal))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.CardSeason))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.LuminaryStateView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.Luminary))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.PlayerState))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.OtherPlayerView))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.WhoseTurn))
    <> (getTypeScriptDeclarations (Proxy :: Proxy GL.RakeTurn))
    )