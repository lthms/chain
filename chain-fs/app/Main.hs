{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad (forever)
import           Control.Monad.Chain
import qualified Control.Monad.Chain.Fs as Fs
import           Control.Monad.IO.Class

readPrintFile :: FilePath -> ResultT String err IO ()
readPrintFile path =
  achieve ("read " ++ path ++ " and print its content to stdout") $ do
    recoverManyWith @[Fs.AlreadyInUse, Fs.DoesNotExist, Fs.AccessDeny, Fs.IllegalOperation] @(Fs.DescriptiveError)
      (do f <- Fs.openFile path Fs.ReadMode
          repeatUntil @(Fs.EoF) (Fs.getLine f >>= liftIO . print)
                                (\_ _ -> liftIO $ putStrLn "%EOF"))
      (\e ctx -> do
          liftIO . putStrLn $ Fs.describe e
          liftIO $ putStrLn "stack:"
          liftIO $ print ctx)

main :: IO ()
main = runResultT $ readPrintFile "hi"
