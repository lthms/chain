{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Monad.Chain
import           Control.Monad.State

type ErrIO err = ResultT String err (StateT Int IO)

myInnerError :: ('[Bool] :| err) => ErrIO err ()
myInnerError = put 1 >> abort True

myErrorFunc :: ErrIO err ()
myErrorFunc =
  achieve "Testing the `chain` package" $ do
    recoverWhile @Bool "hum"
      (myInnerError <?> "myInnerError call")
      printErrorStack

    recoverManyWith @[Bool, String] @Show
          (abort "Meh")
          printErrorStack

printErrorStack :: forall e m msg.
                   (Show e, Show msg, MonadIO m)
                => e
                -> [msg]
                -> m ()
printErrorStack err msgs = do
  liftIO . putStrLn $ "error: " ++ show err
  liftIO $ print msgs

main :: IO ()
main = do
  x <- runStateT (runResultT body) 0
  case x of
    (_, y) -> print y
  where body :: ErrIO '[] ()
        body = myErrorFunc
