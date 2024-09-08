{-# LANGUAGE InstanceSigs #-}
module State where

import Data.Text (Text)
import Data.Char (isSpace)
import qualified Data.Text as T

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f g = State $ \input ->
    let (val, newState) = runState g input
    in (f val, newState)

instance Applicative (State s) where
  pure :: a -> State s a
  pure val = State $ \s -> (val, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  f <*> g = State $ \oldState ->
    let (h, funcState) = runState f oldState
        (val, valState) = runState g funcState
    in (h val, valState)

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  f >>= g = State $ \oldState ->
    let (val, valState) = runState f oldState
    in runState (g val) valState

evalState :: State s a -> s -> a
evalState stateAction initState =
  fst $ runState stateAction initState

execState :: State s a -> s -> s
execState stateAction initState =
  snd $ runState stateAction initState

put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

-- Basic State Demo

appendIntentedLine :: String -> String -> State Int String
appendIntentedLine msg prevMsg = do
  indentLvl <- get
  let
    nextIndentLvl = indentLvl + 2
    indent = replicate nextIndentLvl ' '
    output = prevMsg <> indent <> msg <> "\n"
  put nextIndentLvl
  pure output

appendLineDemo :: IO ()
appendLineDemo =
  putStrLn $ evalState msg 0
  where
    msg = appendIntentedLine "hello" ""
      >>= appendIntentedLine "world"
      >>= appendIntentedLine "love, "
      >>= appendIntentedLine "George"

-- Name Parser Demo

-- newtype State s a = State { runState :: s -> (a, s) }
type ParserS a = State Text (Either Text a)

data FullName = FullName
  { first :: Text
  , middle :: Text
  , last :: Text
  } deriving Show

takeUntil :: (Char -> Bool) -> ParserS Text
takeUntil predicate = do
  oldState <- get
  let (nextVal, rest) = T.break predicate oldState
  put rest
  pure (pure nextVal)

dropChar :: ParserS ()
dropChar = do
  parseState <- get
  let newState = T.tail parseState
  put newState
  pure (Right ())

word :: ParserS Text
word = do
  nextWord <- takeUntil isSpace
  _ <- dropChar
  pure nextWord

parseFullName :: ParserS FullName
parseFullName = do
  firstName <- word
  middleName <- word
  lastName <- word
  pure $ do
    firstName' <- firstName
    middleName' <- middleName
    lastName' <- lastName
    pure $ FullName firstName' middleName' lastName'

