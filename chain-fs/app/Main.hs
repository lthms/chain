{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad          (forever)
import           Control.Monad.Chain
import qualified Control.Monad.Chain.Fs as Fs
import           Control.Monad.IO.Class

readPrintFile :: FilePath -> ResultT String err IO ()
readPrintFile path =
  recoverManyWith @[Fs.AccessError, Fs.IllegalOperation] @Fs.DescriptiveError
    (achieve ("read " ++ path ++ " and print its content to stdout") $ do
         f <- Fs.openFile path Fs.ReadMode
         repeatUntil' @Fs.EoF
           (Fs.getLine f >>= liftIO . print))
    (\e ctx -> do
        liftIO . putStrLn $ Fs.describe e
        liftIO $ putStrLn "stack:"
        liftIO $ print ctx)

main :: IO ()
main = runResultT $ readPrintFile "hi"
