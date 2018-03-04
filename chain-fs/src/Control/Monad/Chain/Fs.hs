{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Chain.Fs
    ( -- * Functions
      withFile
    , closeFile
    , IO.IOMode(..)
    , FileMode(..)
    , Handle
    , stdin
    , stdout
    , stderr
    , DescriptiveError(..)
      -- * Errors
    , AccessError(..)
    , EoF(..)
    , OperationError(..)
    ) where

import           Control.Exception      (handle)
import           Control.Monad.Chain
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import qualified System.IO              as IO
import qualified System.IO.Error        as IO

newtype Handle mode = Handle IO.Handle

stdin :: Handle mode
stdin = Handle IO.stdin

stdout :: Handle mode
stdout = Handle IO.stdout

stderr :: Handle mode
stderr = Handle IO.stderr

class FileMode mode where
  openFile :: ('[AccessError] :| err, MonadIO m)
           => FilePath -> IO.IOMode -> ResultT msg err m (Handle mode)

  getLine :: ('[OperationError, EoF] :| err, MonadIO m)
          => Handle mode
          -> ResultT msg err m mode

  get :: ('[OperationError] :| err, MonadIO m)
      => Handle mode
      -> ResultT msg err m mode

  put :: ('[OperationError] :| err, MonadIO m)
      => Handle mode
      -> mode
      -> ResultT msg err m ()

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

instance DescriptiveError EoF where
  describe _ = "There is nothing left to read"

trySystemIO :: (MonadIO m) => IO a -> m (Either IOError a)
trySystemIO act = liftIO $ handle (pure . Left) $ Right <$> act

withFile :: (FileMode mode, '[AccessError] :| err, MonadIO m)
         => FilePath
         -> IO.IOMode
         -> (Handle mode -> ResultT msg err m a)
         -> ResultT msg err m a
withFile path mode f = do
  h <- openFile path mode
  f h `finally` closeFile h

closeFile :: (MonadIO m)
          => Handle mode
          -> ResultT msg err m ()
closeFile (Handle h) = liftIO $ IO.hClose h

instance FileMode Text where
  openFile path mode = do
    h <- trySystemIO $ IO.openFile path mode
    case h of
      Right handle ->
        pure (Handle handle)
      Left err ->
        abortOnIOError err
      where
        abortOnIOError err
          | IO.isAlreadyInUseError err = abort $ AlreadyInUse path
          | IO.isDoesNotExistError err = abort $ DoesNotExist path
          | IO.isPermissionError err = abort $ AccessDeny path mode
          | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"

  getLine (Handle h) = do
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

  get (Handle h) = do
    str <- trySystemIO $ TIO.hGetContents h
    case str of
      Right str ->
        pure str
      Left err ->
        abortOnIOError err
      where
        abortOnIOError err
          | IO.isEOFError err = pure ""
          | IO.isIllegalOperation err = abort IllegalRead
          | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"

  put (Handle h) txt = do
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

instance FileMode ByteString where
  openFile path mode = do
    h <- trySystemIO $ IO.openBinaryFile path mode
    case h of
      Right handle ->
        pure (Handle handle)
      Left err ->
        abortOnIOError err
      where
        abortOnIOError err
          | IO.isAlreadyInUseError err = abort $ AlreadyInUse path
          | IO.isDoesNotExistError err = abort $ DoesNotExist path
          | IO.isPermissionError err = abort $ AccessDeny path mode
          | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"

  getLine (Handle h) = do
    str <- trySystemIO $ B.hGetLine h
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

  put (Handle h) txt = do
    h <- trySystemIO $ B.hPutStr h txt
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

  get (Handle h) = do
    str <- trySystemIO $ B.hGetContents h
    case str of
      Right str ->
        pure str
      Left err ->
        abortOnIOError err
      where
        abortOnIOError err
          | IO.isEOFError err = pure ""
          | IO.isIllegalOperation err = abort IllegalRead
          | otherwise = error $ show err ++ "\nnote: According to System.IO documentation, this should not happen"
