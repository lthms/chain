{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Main where

import           Control.Monad               (forever)
import           Control.Monad.Chain
import           Control.Monad.Chain.Console as Console
import qualified Control.Monad.Chain.Fs      as Fs
import           Control.Monad.IO.Class
import           Data.Text                   (Text, append, pack)
import           System.Exit

readPrintFile :: ('[ConsoleError] :| err) => FilePath -> ResultT Text err IO ()
readPrintFile path =
  recoverManyWith @[Fs.AccessError, Fs.OperationError] @DescriptiveError
    (achieve ("read " `append` pack path `append` " and print its content to stdout") $
        Fs.withFile path Fs.ReadMode $
          \f -> repeatUntil' @Fs.EoF (Fs.getLine f >>= Console.echo))
    (\e ctx -> do
        Console.log $ pack (Fs.describe e) `append` "\n"
        Console.log "stack:\n"
        mapM_ (\entry -> Console.log $ "* " `append` entry `append` "\n") ctx)

main :: IO ()
main = runResultT $
  recover @ConsoleError (readPrintFile "hi")
                        (\_ _ -> liftIO $ exitWith (ExitFailure 1))
