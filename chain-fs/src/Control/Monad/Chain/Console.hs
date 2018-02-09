{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Chain.Console
  ( echo
  , log
  , ConsoleError(..)
  ) where

import           Control.Monad.Chain
import qualified Control.Monad.Chain.Fs as Fs
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import           Prelude                hiding (log)
import           System.IO              (Handle, stderr, stdout)
import qualified System.IO              as IO

data ConsoleError = StdErrError
                  | StdOutError
                  | StdInError

instance DescriptiveError ConsoleError where
  describe StdOutError = "Could not write text to stdout"
  describe StdErrError = "Could not write text to stderr"
  describe StdInError  = "Could not read text from stdin"

printOrConsoleError :: ('[e] :| err, MonadIO m)
                    => Fs.Handle Text
                    -> Text
                    -> e
                    -> ResultT msg err m ()
printOrConsoleError handle msg err =
  recover @Fs.OperationError
    (Fs.put handle msg)
    (\_ _ -> abort err)

echo :: ('[ConsoleError] :| err, MonadIO m)
     => Text
     -> ResultT msg err m ()
echo msg = printOrConsoleError Fs.stdout msg StdOutError

log :: ('[ConsoleError] :| err, MonadIO m)
    => Text
    -> ResultT msg err m ()
log msg = printOrConsoleError Fs.stderr msg StdErrError
