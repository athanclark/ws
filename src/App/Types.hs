{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , DeriveGeneric
  #-}

module App.Types where

import Network.URI

import Control.Monad.Reader
import Control.Monad.Catch

import System.IO
import System.Exit
import Network.Socket (HostName, PortNumber)

import GHC.Generics


-- * Config Data

data Env = Env
  { envHost :: HostName
  , envPort :: PortNumber
  , envPath :: String
  } deriving (Show, Eq)


-- * Effects Stack

type AppM = ReaderT Env IO

runAppM :: Env -> AppM a -> IO a
runAppM env x = runReaderT x env

type MonadApp m =
  ( MonadReader Env m
  , MonadIO m
  )


-- * Exceptions

data InitException
  = URIParseException String
  | NoURIAuthority String
  deriving (Generic, Show)

instance Exception InitException


handleInitException :: InitException -> IO a
handleInitException e =
  case e of
    URIParseException u -> do
      hPutStr stderr $ "Error: not a valid URI string - `" ++ u ++ "`"
      exitFailure
    NoURIAuthority u -> do
      hPutStr stderr $ "Error: no URI authority - `" ++ u ++ "`"
      exitFailure
