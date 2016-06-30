{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module App where

import App.Types

import Network.URI
import Network.WebSockets
import Wuss

import Data.Text (Text, pack)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad
import Control.Concurrent (forkIO)


app :: MonadApp m => m ()
app = do
  env <- ask
  liftIO $ print env
  liftIO $ runSecureClient
             (envHost env)
             (envPort env)
             (envPath env) ws
  where
    -- totally ripped off from
    -- https://hackage.haskell.org/package/wuss-1.0.4/docs/Wuss.html
    ws conn = do
      void . forkIO . forever $ do
        message <- receiveData conn
        print (message :: Text)

      let loop = do
            userInput <- getLine
            unless (null userInput) $ do
              sendTextData conn (pack userInput)
              loop

      loop

      sendClose conn (pack "Bye!")
