{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

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
    , finally
    , recover
    , recoverWhile
    , recoverMany
    , recoverManyWith
    , recoverManyDescriptive
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
    , eoh
    , HaveInstance(..)
    , DescriptiveError(..)
    ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.IO.Class ()
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

-- | Escape from the ResultT monad.
--
--   The type signature of 'runResultT' obliges the library user to deal with
--   errors, with functions such as 'recover', 'recoverMany', etc.
runResultT :: Monad m => ResultT msg '[] m a -> m a
runResultT chain =
  gRunResultT chain >>= \case
    Right x -> pure x
    Left _  -> error "OneOf '[] is inhabited, so this should not happen"

runResult :: Result msg '[] a -> a
runResult = runIdentity . runResultT

-- | Always execute a given computation (called finally block thereafter) after
--   a first computation (try block) has been completed.
--
--   The 'finally' block is executed even if the 'try' block is aborted. This
--   allows to deal with clean-up code that has to be executed no matter what
--   happened.
finally :: (Monad m)
        => ResultT msg err m a -- ^ The try block
        -> ResultT msg err m () -- ^ The 'finally' block
        -> ResultT msg err m a
finally chain final =
  lift (gRunResultT chain) >>= \case
    Right x -> do
      final
      pure x
    Left (ctx, e) -> do
      final
      throwErr (ctx, e)

-- | Temporally allows one given error type by providing an error handler to
--   execute in case of failure.
recover :: forall e m msg err a.
           (Monad m)
        => ResultT msg (e:err) m a -- ^ The try block
        -> (e -> [msg] -> ResultT msg err m a) -- ^ The error handler
        -> ResultT msg err m a
recover = flip pickError

-- | Combine 'recover' and 'achieve'.
recoverWhile :: forall e m msg err a.
                (Monad m)
             => msg
             -> ResultT msg (e:err) m a
             -> (e -> [msg] -> ResultT msg err m a)
             -> ResultT msg err m a
recoverWhile msg chain f = achieve msg $ recover chain f

-- | Similarly to 'recover', but with more than one error type
--
--   See '+>' and 'eoh' to build the 'Handler'.
recoverMany :: forall plus err m msg a.
               (Split plus err, Monad m)
            => ResultT msg (Join plus err) m a -- ^ The try block
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
  generalize _ = eoh

instance (HaveInstance c rst, c e) => HaveInstance c (e:rst) where
  generalize (f :: forall e. c e => e -> a) = f +> generalize @c @rst @a f

-- | Similarly to 'recoverMany', but use the same error handler for every error
--   types.
--
--   All the error types has to implement a given typeclass.
recoverManyWith :: forall plus c err msg m a.
                   (HaveInstance c plus, Split plus err, Monad m)
                => ResultT msg (Join plus err) m a -- ^ The try block
                -> (forall e. c e => e -> [msg] -> ResultT msg err m a)
                         -- ^ The typeclass-based error handler
                -> ResultT msg err m a
recoverManyWith chain f = recoverMany @plus chain (generalize @c @plus @([msg] -> ResultT msg err m a) f)

recoverManyDescriptive :: forall plus err msg m a.
                          (HaveInstance DescriptiveError plus, Split plus err, Monad m)
                       => ResultT msg (Join plus err) m a
                       -> (forall e. DescriptiveError e => e -> [msg] -> ResultT msg err m a)
                       -> ResultT msg err m a
recoverManyDescriptive = recoverManyWith @plus @DescriptiveError

-- | Declaratively describe the purpose of a computation.
--
--   Using achieve in various places, it becomes possible, once an error is
--   raised, to determine more easily its context. 'achieve' has an operator
--   counterpart: '(<?>)'. The former should be used to contextualise a
--   do-block, whereas the latter can be prefered for monadic function calls.
--
--   > achieve "try to get configuration" $ do
--   >     f <- readParseFile "main" <?> "read main file"
--   >     f' <- readParseFile "aux" <?> "read aux file"
--   >     pure $ buildConfiguration f f'
--
--   These contextual messages are made available, in addition to the error,
--   when using functions such as 'recover', 'recoverMany', 'repeatUntil' etc.
achieve :: (Monad m)
        => msg -- ^ A description of the computation, to act as context in case
               --   of error.
        -> ResultT msg err m a
        -> ResultT msg err m a
achieve msg chain  = do
  res <- lift $ gRunResultT chain
  case res of
    Left (ctx, err) -> throwErr (msg:ctx, err)
    Right x         -> pure x

-- | See 'achieve'.
(<?>) :: (Monad m) => ResultT msg err m a -> msg -> ResultT msg err m a
(<?>) = flip achieve

pickError :: forall e err msg m a.
             ('[e] :| err, Monad m)
          => (e -> [msg] -> ResultT msg (Remove err e) m a)
          -> ResultT msg err m a
          -> ResultT msg (Remove err e) m a
pickError f (ResultT chain) = do
  res <- lift $ runExceptT chain
  case res of
    Left (ctx, err) ->
      case proj err of
        Left e    -> f e ctx
        Right err -> throwErr (ctx, err)
    Right x -> pure x

-- | Repeat a computation which may fail until it fails with a given error.
--
--   Typical use case is reading a file line by line, until reaching its end.
--   If you want to carry some state, you can have a look at 'foldUntil'.
repeatUntil :: forall e err msg m a.
               (Monad m)
            => ResultT msg (e:err) m ()
            -> (e -> [msg] -> ResultT msg err m a)
            -> ResultT msg err m ()
repeatUntil chain f = recover (forever chain) (\err ctx -> void $ f err ctx)

-- | Same as 'repeatUntil', but without an error handler.
repeatUntil' :: forall e err msg m.
                (Monad m)
             => ResultT msg (e:err) m ()
             -> ResultT msg err m ()
repeatUntil' chain = repeatUntil @e chain $ \_ _ -> pure ()

-- | Similarly to 'repeatUntil', repeat a computation until a given error; in
--   addition, carry an accumulator.
foldUntil :: forall e err msg m a.
             (Monad m)
          => a                                         -- ^ Initial state
          -> (a -> ResultT msg (e:err) m a)
          -> (a -> e -> [msg] -> ResultT msg err m a)  -- ^ Error handler
          -> ResultT msg err m a
foldUntil x chain errh = do
  res <- lift $ gRunResultT (chain x)
  case res of
    Right x' -> foldUntil x' chain errh
    Left (ctx, err) -> case proj err of
      Right e -> throwErr (ctx, e)
      Left e  -> errh x e ctx

-- | Same as 'foldUntil', but without the error handler part.
foldUntil' :: forall e err msg m a.
              (Monad m)
           => a                               -- ^ Initial state
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
abort :: ('[e] :| err, Monad m)
      => e -- ^ A symbolic value which describes why the computation would not
           --   be completed. This symbolic value is of a type inside the row of
           --   errors.
      -> ResultT msg err m a
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
