{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Transformers.MonadState (
    MonadState (..),
    module X,
) where

import Transformers.ExceptT
import Transformers.MonadTrans
import Transformers.StateT as X hiding (get, put)
import qualified Transformers.StateT as State

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m => MonadState s (State.StateT s m) where
  put = State.put
  get = State.get

instance MonadState s m => MonadState s (ExceptT e m) where
  put = lift . put
  get = lift get

