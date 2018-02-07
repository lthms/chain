{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import           Prelude                hiding (log)
import           System.IO              (Handle, stderr, stdout)

data ConsoleError = StdOutError
                  | StdInError
                  | StdErrError

instance DescriptiveError ConsoleError where
  describe StdOutError = "Could not write to stdout"
  describe StdInError  = "Could not write to stderr"
  describe StdErrError = "Could not read from stdin"

printOrConsoleError :: ('[ConsoleError] :| err, MonadIO m)
                    => Handle
                    -> Text
                    -> ConsoleError
                    -> ResultT msg err m ()
printOrConsoleError handle msg err =
  recover @Fs.OperationError
    (Fs.put handle msg)
    (\_ _ -> abort err)

echo :: ('[ConsoleError] :| err, MonadIO m)
     => Text
     -> ResultT msg err m ()
echo msg = printOrConsoleError stdout msg StdOutError

log :: ('[ConsoleError] :| err, MonadIO m)
    => Text
    -> ResultT msg err m ()
log msg = printOrConsoleError stderr msg StdErrError
