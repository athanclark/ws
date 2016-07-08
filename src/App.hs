{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module App where

import App.Types

import Network.WebSockets
import Wuss

import qualified Data.Text                  as T
import qualified Data.Text.Lazy.Encoding    as LT
import qualified Data.Text.Lazy.IO          as LT
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Concurrent (forkIO)
import System.IO


app :: MonadApp m => m ()
app = do
  env <- ask
  liftIO $
    if envSecure env
    then runSecureClient
          (envHost env)
          (envPort env)
          (envPath env) ws
    else runClient
          (envHost env)
          (fromIntegral $ envPort env)
          (envPath env) ws
  where
    -- totally ripped off from
    -- https://hackage.haskell.org/package/wuss-1.0.4/docs/Wuss.html
    ws :: ClientApp ()
    ws conn =
      let close = sendClose conn ("Bye!" :: T.Text)
      in  finally close $ do
        -- always listen for incoming messages in a separate thread
        void . forkIO . forever $ do
          message <- receiveDataMessage conn
          let bs = case message of
                    Text   x -> x
                    Binary x -> x
          case LT.decodeUtf8' bs of
            Left e -> do
              hPutStrLn stderr $ "[Warn] Decode Error: " ++ show e
              LBS.putStrLn bs
            Right t ->
              LT.putStrLn t

        -- always listen for outgoing messages in the main thread
        void . forever $ do
          userInput <- LT.getLine
          unless (userInput == "") $
            sendTextData conn userInput
