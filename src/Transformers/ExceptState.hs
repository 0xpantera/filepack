{-# LANGUAGE OverloadedStrings #-}
module Transformers.ExceptState where

import Control.Applicative ()
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Transformers.ExceptT
import Transformers.StateT ()
import Transformers.State
import qualified Transformers.State as S
import Transformers.Identity 

type ParseError = Text
type ParseState = Text

type Parser = ExceptT ParseError (State ParseState)

runParser :: Text -> Parser a -> Either ParseError a
runParser input parser = 
  evalState (runExceptT parser) input

parseChar :: Parser Char
parseChar = do
  parseState <- succeed S.get
  case T.uncons parseState of
    Nothing -> throwError "end of input"
    Just (c, rest) -> do
      succeed $ S.put rest
      pure c

char :: Char -> Parser ()
char expectedC = do
  actualC <- parseChar
  when (expectedC /= actualC) $
    throwError "Invalid Character"

type Except e = ExceptT e Identity

runExcept :: Except e a -> Either e a
runExcept = runIdentity . runExceptT

