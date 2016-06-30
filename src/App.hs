{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module App where

import App.Types

import Network.URI
import Network.WebSockets
import Wuss

import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO       as LT
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad
import Control.Concurrent (forkIO)


app :: MonadApp m => m ()
app = do
  env <- ask
  liftIO $ runSecureClient
             (envHost env)
             (envPort env)
             (envPath env) ws
  where
    -- totally ripped off from
    -- https://hackage.haskell.org/package/wuss-1.0.4/docs/Wuss.html
    ws conn = do
      void . forkIO . forever $ do
        message <- receiveDataMessage conn
        LT.putStrLn . LT.decodeUtf8 $ case message of
                                        Text   x -> x
                                        Binary x -> x

      let loop = do
            userInput <- getLine
            unless (null userInput) $ do
              sendTextData conn (T.pack userInput)
              loop

      loop

      sendClose conn (T.pack "Bye!")
