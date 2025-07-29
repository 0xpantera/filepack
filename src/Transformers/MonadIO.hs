module Transformers.MonadIO (
  MonadIO(..)  
) where

import Transformers.StateT
import Transformers.ExceptT
import Transformers.MonadTrans

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (ExceptT s m) where
    liftIO = lift . liftIO