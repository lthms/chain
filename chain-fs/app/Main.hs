{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

data CatError = CatError

instance DescriptiveError CatError where
  describe _ = "Could not perform a proper cat"

readPrintFile :: ('[CatError, Console.ConsoleError] :| err) => FilePath -> ResultT Text err IO ()
readPrintFile path =
  recoverManyDescriptive @[Fs.AccessError, Fs.OperationError, Console.ConsoleError]
    (achieve ("read " `append` pack path `append` " and print its content to stdout") $
        Fs.withFile @Text path Fs.ReadMode $
          \f -> repeatUntil' @Fs.EoF (Fs.getLine f >>= Console.echo))
    printErrorStack

  where printErrorStack e ctx = do
          Console.log $ pack (Fs.describe e) `append` "\n"
          Console.log "stack:\n"
          mapM_ (\entry -> Console.log $ "* " `append` entry `append` "\n") ctx
          abort CatError

main :: IO ()
main = runResultT $
  recoverMany @[CatError, Console.ConsoleError]
     (readPrintFile "hi") $
        (\_ _ -> liftIO . exitWith $ ExitFailure 1) -- we have printed the reason we failed
     +> (\_ _ -> liftIO . exitWith $ ExitFailure 2) -- we could not
     +> eoh
