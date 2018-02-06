{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Main where

import           Control.Monad               (forever)
import           Control.Monad.Chain
import           Control.Monad.Chain.Console
import qualified Control.Monad.Chain.Fs      as Fs
import           Control.Monad.IO.Class
import           Data.Text                   (Text, append, pack)
import           System.Exit

readPrintFile :: (Contains err ConsoleError) => FilePath -> ResultT Text err IO ()
readPrintFile path =
  recoverManyWith @[Fs.AccessError, Fs.OperationError] @DescriptiveError
    (achieve ("read " `append` pack path `append` " and print its content to stdout") $ do
         f <- Fs.openFile path Fs.ReadMode
         repeatUntil' @Fs.EoF
           (Fs.getLine f >>= echo))
    (\e ctx -> do
        echo $ pack (Fs.describe e) `append` "\n"
        echo "stack:\n"
        mapM_ (\entry -> echo $ "* " `append` entry `append` "\n") ctx)

main :: IO ()
main = runResultT $
  recover @ConsoleError (readPrintFile "hi")
                        (\_ _ -> liftIO $ exitWith (ExitFailure 1))
