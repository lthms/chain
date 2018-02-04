{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
    , (<!>)
    , repeatUntil
      -- * Set of Errors
    , OneOf
    , Contains
    , (:<)
    , (<.>)
    , (<->)
    , HaveInstance(..)
    ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           GHC.Exts (Constraint)

import           Data.TypeSet hiding (cast)

newtype ResultT msg (err :: [*]) m a = ResultT (ExceptT ([msg], OneOf err) m a)
  deriving (Functor, Applicative, Monad)

type Result e a = ResultT e a Identity

instance MonadTrans (ResultT msg err) where
  lift m = ResultT (lift $ m)

instance (MonadIO m) => MonadIO (ResultT msg err m) where
  liftIO = lift . liftIO

instance (Monad m, MonadState s m) => MonadState s (ResultT msg err m) where
  state = lift . state

instance (Monad m, MonadReader env m) => MonadReader env (ResultT msg err m) where
  ask = lift ask
  local f (ResultT chain) = do
    res <- lift . local f $ runExceptT chain
    case res of
      Left (ctx, err) -> throwErr (ctx, err)
      Right x         -> pure x

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
recover = flip handle

class Recover plus where
  recoverMany :: (Monad m) => ResultT msg (Join plus err) m a -> Intos plus ([msg] -> ResultT msg err m a) -> ResultT msg err m a
  cast :: OneOf set -> OneOf (Join plus set)

  liftE :: (Monad m) => ResultT msg err m a -> ResultT msg (Join plus err) m a
  liftE (ResultT chain) = do
    res <- lift $ runExceptT chain
    case res of
      Left (ctx, err) -> do
        throwErr (ctx, cast @plus err)
      Right x -> do
        pure x

instance Recover '[] where
  recoverMany chain Nop = chain
  cast _ = error "should not happen as OneOf '[] is inhabited"

instance (Recover set) => Recover (e:set) where
  cast = Next . (cast @set)

  recoverMany chain (Push (Into f) h) =
    recoverMany @set (handle @e (\e m -> liftE @set $ f e m) chain) h

class HaveInstance c set where
  generalize :: (forall e. c e => e -> a) -> Intos set a

instance HaveInstance c '[] where
  generalize _ = Nop

instance (HaveInstance c rst, c e) => HaveInstance c (e:rst) where
  generalize (f :: forall e. c e => e -> a) = Push (Into f) (generalize @c @rst @a f)

recoverManyWith :: forall plus c m msg err a.
                   (Monad m, Recover plus, HaveInstance c plus)
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

handle :: forall e err msg m a.
          ( Monad m
          , Shrink e err
          , Contains err e)
       => (e -> [msg] -> ResultT msg (Remove err e) m a)
       -> ResultT msg err m a
       -> ResultT msg (Remove err e) m a
handle f (ResultT chain) = do
  res <- lift $ runExceptT chain
  case res of
    Left (ctx, err) ->
      case shrink err of
        Left e    -> f e ctx
        Right err -> throwErr (ctx, err)
    Right x -> pure x

repeatUntil :: forall e err msg m.
               (Monad m)
            => (ResultT msg (e:err) m ())
            -> (e -> [msg] -> ResultT msg err m ())
            -> ResultT msg err m ()
repeatUntil chain f = recover @e (forever chain) f

type family set1 :< set2 :: Constraint where
  '[] :< set2 = ()
  (e:rst) :< set2 = (Contains set2 e, rst :< set2)

-- | Abort the current computation and raise an error to describe the reason
--
--   Similarly to, e.g. 'Nothing' for 'Maybe', 'abort' is a computation
--   shortcut. The rest of the monadic computation is not executed, and the
--   error is transmitted to the caller computation. Using the 'recover',
--   'recoverWhile' or 'recoverMany' functions to stop the chain.
abort :: (Contains err e, Monad m) => e -> ResultT msg err m a
abort e = throwErr ([], inj e)

(<!>) :: forall e err msg m a. (Monad m, Contains err e) => ResultT msg err m a -> (e -> [msg] -> m a) -> ResultT msg err m a
(ResultT chain) <!> f = do
  res <- lift $ runExceptT chain
  case res of
    Left (ctx, err) ->
      case proj @err @e err of
        Just e  -> lift $ f e ctx
        Nothing -> throwErr (ctx, err)
    Right x -> pure x
