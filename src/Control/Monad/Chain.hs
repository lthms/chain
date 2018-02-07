{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Chain
    ( -- * ResultT
      ResultT
    , runResultT
      -- * Result
    , Result
    , runResult
      -- * Monadic Operations
    , abort
    , achieve
    , (<?>)
    , recoverWhile
    , recover
    , recoverMany
    , recoverManyWith
    , repeatUntil
    , repeatUntil'
    , foldUntil
    , foldUntil'
      -- * Leverage Existing Error Handling
    , eitherOr
    , exceptOr
    , orAbort
    , orAbortM
    , orElse
    , orElseM
    , eitherAbort
    , exceptAbort
      -- * Set of Errors
    , Handler
    , (:|)
    , (+>)
    , closeFunction
    , HaveInstance(..)
    , DescriptiveError(..)
    ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe             (fromMaybe)
import           GHC.Exts               (Constraint)

import           Data.TypeSet

newtype ResultT msg (err :: [*]) m a = ResultT (ExceptT ([msg], OneOf err) m a)
  deriving (Functor, Applicative, Monad, MonadReader env, MonadState s, MonadIO, MonadTrans)

type Result e a = ResultT e a Identity

gRunResultT :: Monad m => ResultT msg err m a -> m (Either ([msg], OneOf err) a)
gRunResultT (ResultT chain) = runExceptT chain

throwErr :: (Monad m) => ([msg], OneOf err) -> ResultT msg err m a
throwErr err = ResultT $ throwError err

runResultT :: Monad m => ResultT msg '[] m a -> m a
runResultT chain =
  gRunResultT chain >>= \case
    Right x -> pure x
    Left _  -> error "OneOf '[] is inhabited, so this should not happen"

runResult :: Result msg '[] a -> a
runResult = runIdentity . runResultT

recoverWhile :: forall e m msg err a.
                (Monad m)
             => msg
             -> ResultT msg (e:err) m a
             -> (e -> [msg] -> ResultT msg err m a)
             -> ResultT msg err m a
recoverWhile msg chain f = achieve msg $ recover chain f

recover :: forall e m msg err a.
           (Monad m)
        => ResultT msg (e:err) m a
        -> (e -> [msg] -> ResultT msg err m a)
        -> ResultT msg err m a
recover = flip pickError

recoverMany :: forall plus err m msg a.
               (Split plus err, Monad m)
            => ResultT msg (Join plus err) m a
            -> Handler plus ([msg] -> ResultT msg err m a)
            -> ResultT msg err m a
recoverMany chain (Handler f) = do
  res <- lift $ gRunResultT chain
  case res of
    Right x -> pure x
    Left (ctx, x) -> case split @plus @err x of
      Right x -> throwErr (ctx, x)
      Left e  -> f e ctx

class HaveInstance c set where
  generalize :: (forall e. c e => e -> a) -> Handler set a

instance HaveInstance c '[] where
  generalize _ = closeFunction

instance (HaveInstance c rst, c e) => HaveInstance c (e:rst) where
  generalize (f :: forall e. c e => e -> a) = f +> generalize @c @rst @a f

recoverManyWith :: forall plus c m msg err a.
                   (Monad m, Split plus err, HaveInstance c plus)
                => ResultT msg (Join plus err) m a
                -> (forall e. c e => e -> [msg] -> ResultT msg err m a)
                -> ResultT msg err m a
recoverManyWith chain f = recoverMany @plus chain (generalize @c @plus @([msg] -> ResultT msg err m a) f)

achieve :: (Monad m) => msg -> ResultT msg err m a -> ResultT msg err m a
achieve msg chain  = do
  res <- lift $ gRunResultT chain
  case res of
    Left (ctx, err) -> throwErr (msg:ctx, err)
    Right x         -> pure x

(<?>) :: (Monad m) => ResultT msg err m a -> msg -> ResultT msg err m a
(<?>) = flip achieve

pickError :: forall e err msg m a.
             ( Monad m, Shrink e err, '[e] :| err)
          => (e -> [msg] -> ResultT msg (Remove err e) m a)
          -> ResultT msg err m a
          -> ResultT msg (Remove err e) m a
pickError f (ResultT chain) = do
  res <- lift $ runExceptT chain
  case res of
    Left (ctx, err) ->
      case shrink err of
        Left e    -> f e ctx
        Right err -> throwErr (ctx, err)
    Right x -> pure x

repeatUntil :: forall e err msg m.
               (Monad m)
            => ResultT msg (e:err) m ()
            -> (e -> [msg] -> ResultT msg err m ())
            -> ResultT msg err m ()
repeatUntil chain = recover (forever chain)

repeatUntil' :: forall e err msg m.
                (Monad m)
             => ResultT msg (e:err) m ()
             -> ResultT msg err m ()
repeatUntil' chain = repeatUntil @e chain $ \_ _ -> pure ()

foldUntil :: forall e err msg m a.
             (Monad m)
          => a
          -> (a -> ResultT msg (e:err) m a)
          -> (a -> e -> [msg] -> ResultT msg err m a)
          -> ResultT msg err m a
foldUntil x chain errh = do
  res <- lift $ gRunResultT (chain x)
  case res of
    Right x' -> foldUntil x' chain errh
    Left (ctx, err) -> case shrink err of
      Right e -> throwErr (ctx, e)
      Left e  -> errh x e ctx

foldUntil' :: forall e err msg m a.
              (Monad m)
           => a
           -> (a -> ResultT msg (e:err) m a)
           -> ResultT msg err m a
foldUntil' x chain = foldUntil @e x chain $ \x _ _ -> pure x

type family set1 :| set2 :: Constraint where
  '[] :| set2 = ()
  (e:rst) :| set2 = (Contains set2 e, rst :| set2)

-- | Abort the current computation and raise an error to describe the reason
--
--   Similarly to, e.g. 'Nothing' for 'Maybe', 'abort' is a computation
--   shortcut. The rest of the monadic computation is not executed, and the
--   error is transmitted to the caller computation. Using the 'recover',
--   'recoverWhile' or 'recoverMany' functions to stop the chain.
abort :: ('[e] :| err, Monad m) => e -> ResultT msg err m a
abort e = throwErr ([], inj e)

orAbortM :: ('[e] :| err, Monad m)
         => m (Maybe a)
         -> e
         -> ResultT msg err m a
orAbortM c e = lift c >>= (`orAbort` e)

orAbort :: ('[e] :| err, Monad m)
        => Maybe a
        -> e
        -> ResultT msg err m a
orAbort (Just x) e = pure x
orAbort _        e = abort e

eitherAbort :: ('[e] :| err, Monad m)
            => Either e a
            -> ResultT msg err m a
eitherAbort (Right x) = pure x
eitherAbort (Left e)  = abort e

exceptAbort :: ('[e] :| err, MonadError e m)
            => m a
            -> ResultT msg err m a
exceptAbort c = eitherToExcept c >>= eitherAbort

eitherOr :: (Monad m)
         => Either e a
         -> a
         -> ResultT msg err m a
eitherOr (Right x) _ = pure x
eitherOr _         x = pure x

exceptOr :: (Monad m, MonadError e m)
         => m a
         -> a
         -> ResultT msg err m a
exceptOr c e = eitherToExcept c >>= (`eitherOr` e)

orElse :: (Monad m)
       => Maybe a
       -> a
       -> ResultT msg err m a
orElse m x = pure (fromMaybe x m)

orElseM :: (Monad m)
        => m (Maybe a)
        -> a
        -> ResultT msg err m a
orElseM c x = lift c >>= (`orElse` x)

eitherToExcept :: (MonadError e m)
               => m a
               -> ResultT msg err m (Either e a)
eitherToExcept m = lift ((Right <$> m) `catchError` (pure . Left))

class DescriptiveError err where
  describe :: err -> String
