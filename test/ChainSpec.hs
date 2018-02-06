{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module ChainSpec where

import           Control.Monad.Chain hiding (describe)
import qualified Control.Monad.Chain (describe)
import           Test.Hspec

spec :: Spec
spec =
  describe "The ResultT Monad" $ do
    it "should deal with stack of messages" $
      runResult achieveStacksMessages
        `shouldBe` (["message1", "message2"], Error1)

    it "should deal with an inner error" $
      runResult achieveRecoverInner
        `shouldBe` []

    it "should deal with an outer error" $
      runResult achieveRecoverOuter
        `shouldBe` ["message"]

    it "should deal many errors" $
      runResult recoverManyErrors
        `shouldBe` "Error3"

    it "should deal many errors" $
      runResult foldShouldWork
        `shouldBe` 5

-- Tests functions
foldShouldWork :: Result msg err Int
foldShouldWork =
  foldUntil' @Error1 0 (\x -> if x == 5
                              then abort Error1
                              else pure (x + 1))

recoverManyErrors :: Result String err String
recoverManyErrors =
  recoverManyWith @[Error1, Error2, Error3] @Show
    (abort Error3)
    (\e _ -> pure $ show e)

achieveStacksMessages :: Result String err ([String], Error1)
achieveStacksMessages =
  recover @Error1
    (achieve "message1" $ achieve "message2" $ abort Error1)
    (\e msg -> pure (msg, e))

achieveRecoverInner :: Result String err [String]
achieveRecoverInner =
  recover @Error1
      (recoverWhile @Error2 "message"
           (abort Error2)
           (\_ msg -> pure msg))
      (\_ msg -> pure msg)

achieveRecoverOuter :: Result String err [String]
achieveRecoverOuter =
  recover @Error1
      (recoverWhile @Error2 "message"
           (abort Error1)
           (\_ msg -> pure msg))
      (\_ msg -> pure msg)

-- Error Types
data Error1 = Error1
  deriving (Eq, Show)

data Error2 = Error2
  deriving (Eq, Show)

data Error3 = Error3
  deriving (Eq, Show)
