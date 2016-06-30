{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Main where

import App
import App.Types
import Options.Applicative
import Network.URI

import Data.Maybe
import Data.Monoid
import Text.Read
import Control.Monad.Reader
import Control.Monad.Catch


-- * Options Parsing

-- | Application-wide options
data AppOpts = AppOpts
  { url :: String
  }

appOpts :: Parser AppOpts
appOpts = AppOpts
  <$> strOption
        ( long "url"
       <> short 'u'
       <> metavar "TARGET"
       <> help "The websocket address to connect to - example:\
               \ `ws://localhost:3000/foo`" )


-- * Executable

main :: IO ()
main = do
  let opts :: ParserInfo AppOpts
      opts = info (helper <*> appOpts)
        ( fullDesc
       <> progDesc "Connect to a websocket"
       <> header "ws - a CLI websocket tool" )

  env <- handle handleInitException
       $ appOptsToEnv =<< execParser opts

  runAppM env app




-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> IO Env
appOptsToEnv (AppOpts u) =
  case parseURI u of
    Nothing -> throwM $ URIParseException u
    Just u' ->
      case uriAuthority u' of
        Nothing -> throwM $ NoURIAuthority u
        Just a ->
          let host = uriUserInfo a
                  ++ uriRegName a
              port = fromIntegral $
                let ps = drop 1 (uriPort a)
                in if ps /= ""
                then read ps
                else if uriScheme u' == "wss:"
                then 443
                else 80
              path =
                let p = uriPath u'
                in if p == ""
                then "/"
                else p
          in  pure Env { envHost = host
                       , envPort = port
                       , envPath = path
                       }
