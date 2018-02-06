{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Control.Monad.Chain.Console
  ( echo
  , ConsoleError(..)
  ) where

import           Control.Monad.Chain
import qualified Control.Monad.Chain.Fs as Fs
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import           System.IO              (stderr, stdin, stdout)

data ConsoleError = StdOutError
                  | StdInError
                  | StdErrError

instance DescriptiveError ConsoleError where
  describe StdOutError = "Could not write to stdout"
  describe StdInError  = "Could not write to stderr"
  describe StdErrError = "Could not read from stdin"

echo :: (Contains err ConsoleError, MonadIO m)
     => Text
     -> ResultT msg err m ()
echo msg =
  recover @Fs.OperationError
    (Fs.put stdout msg)
    (\_ _ -> abort StdOutError)
