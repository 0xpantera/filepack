module ExceptTParser where

import ExceptT
import Control.Monad
import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified State

-- newtype State s a = State { runState :: s -> (a, s) }
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- ExceptT String (State Text) a
type Parser a = ExceptT String (State.State Text) a

runParser :: Parser a -> Text -> Either String a
runParser = State.evalState . runExceptT

parseNextChar :: Parser Char
parseNextChar = do
  input <- succeed State.get
  when (T.null input) $
    throwError "parseNextChar: unexpected end of input"
  succeed . State.put . T.tail $ input
  pure $ T.head input

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- succeed State.get
  let (nextVal, rest) = T.break predicate oldState
  succeed . State.put $ rest
  pure nextVal

dropChar :: Parser ()
dropChar = do
  parseState <- succeed State.get
  case T.uncons parseState of
    Nothing -> throwError "unexpected end of input"
    Just (_, rest) -> succeed $ State.put rest

word :: Parser Text
word = do
  nextWord <- takeUntil isSpace
  when (T.null nextWord) $
    throwError "unexpected end of input"
  ignoreException dropChar
  pure nextWord
  where
    ignoreException =
      catchError (const $ pure ())