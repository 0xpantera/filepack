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
import Transformers.MonadError
import Transformers.MonadIO
import Transformers.MonadState


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
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadState Text
    , MonadError Text
    , MonadIO
    )

runArchiver :: Text -> Archiver a -> IO (Either Text a)
runArchiver inputTxt archiver =
  runExceptT $ evalStateT (unArchiver archiver) inputTxt

parseChar :: Archiver Char
parseChar = do
  parseTxt <- get
  case T.uncons parseTxt of
    Nothing ->
      throwError "end of input"
    Just (c, rest) -> do
      put rest
      pure c

dropSpaces :: Archiver ()
dropSpaces = void $ many (expectChar ' ')

isNewline :: Char -> Bool
isNewline = (== '\n')

expect :: Eq a => Archiver a -> a -> Archiver ()
expect getActual expected = do
  actual <- getActual
  when (expected /= actual) $ do
    throwError "expectation violated"

expectChar :: Char -> Archiver ()
expectChar = expect parseChar

expectText :: Text -> Archiver ()
expectText expected = do
  stripped <- T.stripPrefix expected <$> get
  case stripped of
    Nothing -> throwError "missing expected string"
    Just rest -> put rest

takeUntil :: (Char -> Bool) -> Archiver Text
takeUntil predicate = do
  (result, rest) <- T.break predicate <$> get
  put rest
  pure result

word :: Archiver Text
word = do
  nextWord <- takeUntil (\s -> isSeparator s || isNewline s)
  void . optional $ expectChar '\n' <|> dropSpaces
  when (T.null nextWord) $
    throwError "end of input"
  pure nextWord

quotedString :: Archiver Text
quotedString = do
  expectChar '"'
  quotedText <- takeUntil (== '"')
  expectChar '"'
  pure quotedText

restOfLine :: Archiver Text
restOfLine = remainderOfLine <|> remainderOfText
  where
    remainderOfLine = do
      txt <- takeUntil isNewline
      expectChar '\n'
      pure txt
    remainderOfText = get

dropEmptyLines :: Archiver ()
dropEmptyLines =
  void $ many $ dropSpaces >> expectChar '\n'

parseIndentedLine :: Int -> Archiver Text
parseIndentedLine indentLvl = do
  expectText $ T.replicate indentLvl " "
  restOfLine

runSubparser :: Archiver a -> Text -> Archiver a
runSubparser action subState = do
  oldTxt <- get
  put subState
  result <- action
  put oldTxt
  pure result

parseBlock :: Archiver a -> Archiver a
parseBlock blockParser =
  dropEmptyLines >> getBlock >>= runSubparser blockParser
  where
    getBlock = do
      fstLineSpacing <- takeUntil (not . isSeparator)
      let indentation = T.length fstLineSpacing
      fstLine <- restOfLine
      restOfBlock <- many (dropEmptyLines >> parseIndentedLine indentation)
      pure $ T.unlines (fstLine : restOfBlock)

parseImportStatement :: Archiver ArchivedFile
parseImportStatement = do
  expectText "import"
  dropSpaces
  path <- quotedString
  dropSpaces
  expectChar '\n'
  contents <- liftIO $ BS.readFile (T.unpack path)
  pure $ ArchivedFile path contents

parseNewFileStatement :: Archiver ArchivedFile
parseNewFileStatement = do
  expectText "new-file"
  dropSpaces
  path <- quotedString
  dropSpaces
  expectText ":\n"
  body <- Enc.encodeUtf8 <$> parseBlock get
  pure $ ArchivedFile path body

parseArchiveStatements :: Archiver [ArchivedFile]
parseArchiveStatements =
  many $ dropEmptyLines >> (parseImportStatement <|> parseNewFileStatement)

parseArchive :: Archiver Archive
parseArchive = do
  expectText "archive"
  dropSpaces
  archiveName <- quotedString
  expectText ":\n"
  files <- parseBlock parseArchiveStatements
  pure $ Archive archiveName files