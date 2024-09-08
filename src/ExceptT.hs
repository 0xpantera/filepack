{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
module ExceptT where

import Control.Applicative
import Data.Kind (Type)

newtype ExceptT (e :: Type) (m :: Type -> Type) (a :: Type) = 
    ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
    fmap :: Functor m => (a -> b) -> ExceptT e m a -> ExceptT e m b
    fmap f a = ExceptT $ (fmap . fmap) f (runExceptT a)

instance Monad m => Applicative (ExceptT e m) where
    pure :: Monad m => a -> ExceptT e m a
    pure val = ExceptT $ pure (pure val)

    (<*>) :: Monad m => ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
    ef <*> ea = ExceptT $ do
      f <- runExceptT ef
      a <- runExceptT ea
      pure (f <*> a)

instance Monad m => Monad (ExceptT e m) where
    return :: Monad m => a -> ExceptT e m a
    return = pure

    (>>=) :: ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
    ea >>= ef = ExceptT $ do
      a <- runExceptT ea
      case a of
        Left err -> pure (Left err)
        Right a' -> runExceptT $ ef a'

throwError :: Monad m => e -> ExceptT e m a
throwError exception = ExceptT (pure $ Left exception)

catchError :: Monad m => (e -> ExceptT e m a) -> ExceptT e m a -> ExceptT e m a
catchError handler action = ExceptT $ do
  result <- runExceptT action
  case result of
    Left err -> runExceptT $ handler err
    Right a -> pure (Right a)

succeed :: Monad m => m a -> ExceptT e m a
succeed a = ExceptT (Right <$> a)
