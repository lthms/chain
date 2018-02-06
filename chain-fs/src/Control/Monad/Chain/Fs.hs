{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Chain.Fs
    ( -- * Functions
      openFile
    , closeFile
    , getLine
    , put
    , IO.IOMode(..)
    , DescriptiveError(..)
      -- * Errors
    , AccessError(..)
    , EoF(..)
    , OperationError(..)
    ) where

import           Control.Exception
import           Control.Monad.Chain
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import           Prelude                hiding (getLine)
import qualified System.IO              as IO
import qualified System.IO.Error        as IO

data AccessError = AlreadyInUse FilePath
                 | DoesNotExist FilePath
                 | AccessDeny FilePath IO.IOMode

data EoF = EoF

data OperationError = IllegalRead
                    | IllegalWrite
                    | FullDevice

instance DescriptiveError AccessError where
  describe (AlreadyInUse path) = "File " ++ path ++ " is already used by something else"
  describe (AccessDeny path mode) = "Accesses " ++ show mode ++ " were not enough to work with " ++ path
  describe (DoesNotExist path) = "File " ++ path ++ " does not exist"

instance DescriptiveError OperationError where
  describe IllegalRead = "Attempt to read a file which has not been opened to be read"
  describe IllegalWrite = "Attempt to write a file which has not been opened to be read"
  describe FullDevice = "The device is full"

trySystemIO :: (MonadIO m) => IO a -> m (Either IOError a)
trySystemIO act = liftIO $ handle (pure . Left) $ Right <$> act

openFile :: (Contains err AccessError, MonadIO m)
         => FilePath -> IO.IOMode -> ResultT msg err m IO.Handle
openFile path mode = do
  h <- trySystemIO $ IO.openFile path mode
  case h of
    Right handle ->
      pure handle
    Left err ->
      abortOnIOError err
  where
    abortOnIOError err
      | IO.isAlreadyInUseError err = abort $ AlreadyInUse path
      | IO.isDoesNotExistError err = abort $ DoesNotExist path
      | IO.isPermissionError err = abort $ AccessDeny path mode
      | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"

closeFile :: (MonadIO m)
          => IO.Handle
          -> ResultT msg err m ()
closeFile h = liftIO $ IO.hClose h

getLine :: ('[OperationError, EoF] :< err, MonadIO m)
        => IO.Handle
        -> ResultT msg err m Text
getLine h = do
  str <- trySystemIO $ TIO.hGetLine h
  case str of
    Right str ->
      pure str
    Left err ->
      abortOnIOError err
  where
    abortOnIOError err
      | IO.isEOFError err = abort EoF
      | IO.isIllegalOperation err = abort IllegalRead
      | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"

put :: ('[OperationError] :< err, MonadIO m)
    => IO.Handle
    -> Text
    -> ResultT msg err m ()
put h txt = do
  h <- trySystemIO $ TIO.hPutStr h txt
  case h of
    Right _ ->
      pure ()
    Left err ->
      abortOnIOError err
  where
    abortOnIOError err
      | IO.isIllegalOperation err = abort IllegalWrite
      | IO.isFullError err = abort FullDevice
      | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"
