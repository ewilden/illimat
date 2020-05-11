{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Adapter.InMemory.Game
import Import
import Run

import RIO.Process
import Options.Applicative.Simple

import qualified Paths_illimatrio

-- for testing
mkApp :: IO App
mkApp = do
  let options = Options False 3003 "testpassword"
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  tvar <- newTVarIO Adapter.InMemory.Game.initialState
  (lf, destructor :: IO ()) <- newLogFunc lo
  return $ App 
    { appLogFunc = lf
    , appProcessContext = pc
    , appOptions = options
    , appGameRelatedState = tvar
    }


main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_illimatrio.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> option auto ( long "port"
                      <> short 'p'
                      <> help "Which port to run the API on."
                      <> value 3003
                        )
       <*> option auto ( long "userCipherKey"
                      <> short 'k'
                      <> help "What password to use to validate UserIds."
                      )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  tvar <- newTVarIO Adapter.InMemory.Game.initialState
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appGameRelatedState = tvar
          }
    in runRIO app run
