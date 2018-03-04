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
  , scan
  , ConsoleError(..)
  ) where

import           Control.Monad.Chain
import qualified Control.Monad.Chain.Fs as Fs
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text.IO           as TIO
import           Prelude                hiding (log)
import           System.IO              (Handle, stderr, stdin, stdout)
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

-- | Write 'Text' to stdout.
echo :: ('[ConsoleError] :| err, MonadIO m)
     => Text
     -> ResultT msg err m ()
echo msg = printOrConsoleError Fs.stdout msg StdOutError

-- | Write 'Text' to stderr.
log :: ('[ConsoleError] :| err, MonadIO m)
    => Text
    -> ResultT msg err m ()
log msg = printOrConsoleError Fs.stderr msg StdErrError

-- | Read one line of 'Text' from stdin.
scan :: ('[ConsoleError] :| err, MonadIO m)
     => ResultT msg err m Text
scan = recoverMany @[Fs.OperationError, Fs.EoF]
         (Fs.getLine Fs.stdin)
         ((\_ _ -> abort StdInError)
          +> (\ _ _ -> abort StdInError)
          +> eoh)
