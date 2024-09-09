module Transformers.Identity where

newtype Identity a = Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f a = Identity $ f (runIdentity a)

instance Applicative Identity where
  pure = Identity

  f <*> a = Identity $ runIdentity f (runIdentity a)

instance Monad Identity where
  return = pure

  a >>= f = f (runIdentity a)