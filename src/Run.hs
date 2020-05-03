{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import qualified RIO.Text.Lazy as TL
import Web.Scotty.Trans

import Import

run :: RIO App ()
run = do
  logInfo "Starting application"
  port <- getOption optionsPort
  app <- ask
  scottyT port (runRIO app) routes

routes :: ScottyT TL.Text (RIO App) ()
routes = do
  get "/" $ text "hello"
  get "/:name" $ do
    name <- param "name"
    text $ "hello " <> name
