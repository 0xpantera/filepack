{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Transformers.Archiver where

import Control.Applicative
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSeparator)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Transformers.ExceptT
import Transformers.StateT

data Archive = Archive
  { archiveName :: Text
  , arvhicedFiles :: [ArchivedFile]
  } deriving stock (Show)

data ArchivedFile = ArchivedFile
  { archivedFileName :: Text
  , archivedFileContents :: ByteString
  } deriving stock (Show)

newtype Archiver a = Archiver
  { unArchiver :: StateT Text (ExceptT Text IO) a }
  deriving newtype (Functor, Applicative, Monad, Alternative)

runArchiver :: Text -> Archiver a -> IO (Either Text a)
runArchiver inputTxt archiver =
  runExceptT $ evalStateT (unArchiver archiver) inputTxt

parseChar :: Archiver Char
parseChar = do
  parseTxt <- Archiver get
  case T.uncons parseTxt of
    Nothing ->
      Archiver . liftStateT . throwError $ "end of input"
    Just (c, rest) -> do
      Archiver (put rest)
      pure c

