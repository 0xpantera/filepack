{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module FilePack.Decoding where

import Data.Bits ((.|.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Applicative
import Control.Monad ( when )
import Data.Word (Word8, Word16, Word32)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Posix.Types (FileMode, CMode(..))


class Decode a where
  decode :: ByteString -> Either String a

instance Decode ByteString where
  decode = Right

instance Decode Text where
  decode = Right . decodeUtf8

instance Decode String where
  decode = Right . BC.unpack

instance Decode Word32 where
  decode = bytestringToWord32

instance Decode Word16 where
  decode = bytestringToWord16

instance Decode FileMode where
  decode bs = do
    w32 <- decode bs
    return $ fromIntegral (w32 :: Word32)


instance (Decode a, Decode b) => Decode (a, b) where
  decode = execParser $ (,) <$> extractValue <*> extractValue


word16FromBytes :: (Word8, Word8) -> Word16
word16FromBytes (a,b) =
  let
    a' = fromIntegral a
    b' = shift (fromIntegral b) 8
  in a' .|. b'

bytestringToWord16 :: ByteString -> Either String Word16
bytestringToWord16 bytestring =
  case BS.unpack bytestring of
    [a,b] -> Right $ word16FromBytes (a,b)
    _otherwise ->
      let l = show $ BS.length bytestring
      in Left ("Expecting 2 bytes but got " <> l)

word32FromBytes :: (Word8, Word8, Word8, Word8) -> Word32
word32FromBytes (a,b,c,d) =
  let
    a' = fromIntegral a
    b' = shift (fromIntegral b) 8
    c' = shift (fromIntegral c) 16
    d' = shift (fromIntegral d) 24
  in a' .|. b' .|. c' .|. d'

bytestringToWord32 :: ByteString -> Either String Word32
bytestringToWord32 bytestring =
  case BS.unpack bytestring of
    [a,b,c,d] -> Right $ word32FromBytes (a,b,c,d)
    _otherwise ->
      let l = show $ BS.length bytestring
      in Left ("Expecting 4 bytes but got " <> l)

naiveDecodeWord32 :: ByteString -> Either String (Word32, ByteString)
naiveDecodeWord32 inputStr = do
  when (BS.length inputStr < 4) $
    Left "Error, not enough data to get the size of the next field"
  let (encodedSizePrefix, rest) = BS.splitAt 4 inputStr
  sizePrefix <- fromIntegral <$> bytestringToWord32 encodedSizePrefix
  when (sizePrefix /= 4) $
    Left "The field size of a word should be 4"
  when (BS.length rest < fromIntegral sizePrefix) $
    Left "Not enough data for the next field size"
  let (encodedWord, rest') = BS.splitAt sizePrefix rest
  decodedWord <- decode encodedWord
  pure (decodedWord, rest')

extractBytes :: Int -> ByteString -> Either String (ByteString, ByteString)
extractBytes n bs = do
  when (BS.length bs < n) $
    Left $ "Error, extract bytes needs at least " <> show n <> " bytes"
  pure $ BS.splitAt n bs

nextSegmentSize :: ByteString -> Either String (Word32, ByteString)
nextSegmentSize bs = do
  (nextSegmentStr, rest) <- extractBytes 4 bs
  parsedSegmentSize <- bytestringToWord32 nextSegmentStr
  pure (parsedSegmentSize, rest)

nextSegment :: ByteString -> Either String (ByteString, ByteString)
nextSegment bs = do
  (segmentSize, rest) <- nextSegmentSize bs
  extractBytes (fromIntegral segmentSize) rest

newtype FilePackParser a = FilePackParser
  { runParser :: ByteString -> Either String (a, ByteString) }

instance Functor FilePackParser where
  fmap :: (a -> b) -> FilePackParser a -> FilePackParser b
  fmap f parser = FilePackParser $ \input -> do
    (parsedValue, result) <- runParser parser input
    pure (f parsedValue, result)

instance Applicative FilePackParser where
  pure :: a -> FilePackParser a
  pure a = FilePackParser $ \s -> pure (a, s)

  (<*>) :: FilePackParser (a -> b) -> FilePackParser a -> FilePackParser b
  f <*> s = FilePackParser $ \input -> do
    (f', initialRemainder) <- runParser f input
    (a, finalRemainder) <- runParser s initialRemainder
    pure (f' a, finalRemainder)

extractValue :: Decode a => FilePackParser a
extractValue = FilePackParser $ \input -> do
  when (BS.length input < 4) $
    Left "Input has less than 4 bytes, we can't get a segment size"
  
  let (rawSegmentSize, rest) = BS.splitAt 4 input
  segmentSize <- fromIntegral <$> bytestringToWord32 rawSegmentSize

  when (BS.length rest < segmentSize) $
    Left "not enough input to parse the next value"

  let (rawSegmentVal, rest') = BS.splitAt segmentSize rest

  case decode rawSegmentVal of
    Left err -> Left err
    Right a -> Right (a, rest')

execParser :: FilePackParser a -> ByteString -> Either String a
execParser parser inputStr = fst <$> runParser parser inputStr

instance Alternative FilePackParser where
  empty = FilePackParser $ const (Left "empty parser")
  parserA <|> parserB = FilePackParser $ \s ->
    case runParser parserA s of
      Right val -> Right val
      Left _ -> runParser parserB s

instance {-# OVERLAPPABLE #-} Decode a => Decode [a] where
  decode = execParser (many extractValue)

instance Monad FilePackParser where
  return :: a -> FilePackParser a
  return = pure

  (>>=) :: FilePackParser a -> (a -> FilePackParser b) -> FilePackParser b
  valParser >>= mkParser = FilePackParser $ \input -> do
    (val, rest) <- runParser valParser input
    runParser (mkParser val) rest

instance MonadFail FilePackParser where
  fail :: String -> FilePackParser a
  fail err = FilePackParser (const $ Left err)