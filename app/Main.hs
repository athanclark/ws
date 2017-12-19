{-# LANGUAGE
    ScopedTypeVariables
  , FlexibleContexts
  #-}

module Main where

import App (app)
import App.Types (Env (Env, envHost, envPort, envPath, envSecure), runAppM, handleInitException, InitException (NoURIAuthority, URIParseException))
import Options.Applicative (Parser, strArgument, ParserInfo, metavar, help, fullDesc, progDesc, header, helper, info, execParser)
import Network.URI (parseURI, uriAuthority, uriRegName, uriUserInfo, uriScheme, uriPath, uriPort)

import Data.Monoid ((<>))
import Control.Monad.Catch (throwM, handle)
import Control.Applicative (optional)


-- * Options Parsing

-- | Application-wide options
newtype AppOpts = AppOpts
  { url :: Maybe String
  }

-- | Options for each field
appOpts :: Parser AppOpts
appOpts =
  AppOpts <$> urlOpt
  where
    urlOpt :: Parser (Maybe String)
    urlOpt = optional $ strArgument $
         metavar "TARGET"
      <> help "The websocket address to connect to - example:\
                \ `ws://localhost:3000/foo`"


-- | Options for entire app
opts :: ParserInfo AppOpts
opts = info (helper <*> appOpts) $
    fullDesc
 <> progDesc "Connect to a websocket"
 <> header "ws - a CLI websocket tool"



-- * Executable

main :: IO ()
main = do
  env <- handle handleInitException
       $ appOptsToEnv =<< execParser opts

  runAppM env app



-- | Translate the CLI parsed options to a sane type we can use in our app
appOptsToEnv :: AppOpts -> IO Env
appOptsToEnv (AppOpts mu) = do
  u <- case mu of
    Nothing -> do
      putStrLn "Websocket URI: "
      getLine
    Just x -> pure x
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
                then 443 :: Int
                else 80
              path' =
                let p = uriPath u'
                in if p == ""
                then "/"
                else p
          in  pure Env { envHost   = host
                       , envPort   = port
                       , envPath   = path'
                       , envSecure = uriScheme u' == "wss:"
                       }
