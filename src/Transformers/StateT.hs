{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module Transformers.StateT where

import Control.Applicative

newtype StateT s m a = 
    StateT { runStateT :: s -> m (a, s) }

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT stateAction initState =
  fst <$> runStateT stateAction initState

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT stateAction initState =
  snd <$> runStateT stateAction initState

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f s = 
    StateT $ fmap (first f) . runStateT s
    where first g (a,b) = (g a, b)

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: Monad m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  f <*> a = StateT $ \s -> do
    (g,s') <- runStateT f s
    (b,s'') <- runStateT a s'
    pure (g b, s'')

instance Monad m => Monad (StateT s m) where
  return :: Monad m => a -> StateT s m a
  return = pure
  
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  a >>= f = StateT $ \s -> do
    (b, s') <- runStateT a s
    runStateT (f b) s'

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ const empty

  a <|> b = StateT $ \s ->
    runStateT a s <|> runStateT b s

put :: Monad m => s -> StateT s m ()
put state = StateT $ \_ -> pure ((), state)

get :: Monad m => StateT s m s
get = StateT $ \state -> pure (state, state)

liftStateT :: Monad m => m a -> StateT s m a
liftStateT a = StateT $ \s -> (, s) <$> a