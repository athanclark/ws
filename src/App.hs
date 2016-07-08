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
import Control.Concurrent.Async
import System.IO
import System.Exit


app :: MonadApp m => m ()
app = do
  env <- ask
  liftIO $
    handle handleConnException $
      if envSecure env
      then runSecureClient
            (envHost env)
            (envPort env)
            (envPath env)
            ws
      else runClient
            (envHost env)
            (fromIntegral $ envPort env)
            (envPath env)
            ws
  where
    -- totally ripped off from
    -- https://hackage.haskell.org/package/wuss-1.0.4/docs/Wuss.html
    ws :: ClientApp ()
    ws conn = do
      -- always listen for incoming messages in a separate thread
      let listen = forever $ do
            message <- receiveDataMessage conn
            let bs = case message of
                      Text   x -> x
                      Binary x -> x
            case LT.decodeUtf8' bs of
              Left e -> do
                hPutStrLn stderr $ "[Warn] UTF8 Decode Error: " ++ show e
                LBS.putStrLn bs
              Right t ->
                LT.putStrLn t

      -- always listen for outgoing messages in the main thread
      let sender = forever $ do
            userInput <- LT.getLine
            unless (userInput == "") $
              sendTextData conn userInput

      withAsync listen $ \l ->
        withAsync sender $ \s -> do
          void $ wait l
          void $ wait s

      sendClose conn ("Bye from ws!" :: T.Text)


handleConnException :: ConnectionException -> IO a
handleConnException e =
  case e of
    CloseRequest c m -> do
      hPutStrLn stderr $ "[Info] Closing with code " ++ show c
                      ++ " and message " ++ show m
      exitSuccess
    ConnectionClosed -> do
      hPutStrLn stderr "[Error] Connection closed unexpectedly"
      exitFailure
    ParseException s -> do
      hPutStrLn stderr $ "[Error] Websocket stream parse failure: " ++ s
      exitFailure
