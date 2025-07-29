{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Transformers.MonadError (
    MonadError (..),
    module X,
) where

import Transformers.ExceptT as X hiding (catchError, throwError)
import qualified Transformers.ExceptT as Except
import Transformers.MonadTrans
--import ReaderT
import Transformers.StateT

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance Monad m => MonadError e (Except.ExceptT e m) where
  throwError = Except.throwError
  catchError = flip Except.catchError

instance MonadError e m => MonadError e (StateT s m) where
  throwError :: MonadError e m => e -> StateT s m a
  throwError = lift . throwError
  
  catchError :: MonadError e m => StateT s m a -> (e -> StateT s m a) -> StateT s m a
  catchError action handler =
    StateT $ \s ->
      let inner = runStateT action s
          liftedHandler e = runStateT (handler e) s
      in catchError inner liftedHandler

