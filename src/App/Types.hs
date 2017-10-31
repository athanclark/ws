{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , DeriveGeneric
  #-}

module App.Types where

import Control.Monad.Reader (ReaderT (runReaderT), MonadReader)
import Control.Monad.Catch (Exception)
import Control.Monad.IO.Class (MonadIO)

import System.IO (hPutStr, stderr)
import System.Exit (exitFailure)
import System.Console.Haskeline (InputT, runInputT, defaultSettings)
import Network.Socket (HostName, PortNumber)

import GHC.Generics (Generic)


-- * Config Data

data Env = Env
  { envHost    :: HostName
  , envPort    :: PortNumber
  , envPath    :: String
  , envSecure  :: Bool
  } deriving (Show, Eq)


-- * Effects Stack

type AppM = InputT (ReaderT Env IO)

runAppM :: Env -> AppM a -> IO a
runAppM env x = runReaderT (runInputT defaultSettings x) env


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
