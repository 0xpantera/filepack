module Transformers.ExceptTParser where

import Transformers.ExceptT
import Control.Monad
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Transformers.State as S

-- newtype State s a = State { runState :: s -> (a, s) }
-- newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
-- ExceptT String (State Text) a
type Parser a = ExceptT String (S.State Text) a

runParser :: Parser a -> Text -> Either String a
runParser = S.evalState . runExceptT

parseNextChar :: Parser Char
parseNextChar = do
  input <- succeed S.get
  when (T.null input) $
    throwError "parseNextChar: unexpected end of input"
  succeed . S.put . T.tail $ input
  pure $ T.head input

takeUntil :: (Char -> Bool) -> Parser Text
takeUntil predicate = do
  oldState <- succeed S.get
  let (nextVal, rest) = T.break predicate oldState
  succeed . S.put $ rest
  pure nextVal

dropChar :: Parser ()
dropChar = do
  parseState <- succeed S.get
  case T.uncons parseState of
    Nothing -> throwError "unexpected end of input"
    Just (_, rest) -> succeed $ S.put rest

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