{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module FilePack where

import Data.Bits ((.&.), (.|.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import Data.Word
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as T
import System.Posix.Types (FileMode, CMode(..))
import Text.Read (readEither)

data FileContents 
  = StringFileContents String
  | TextFileContents Text
  | ByteStringFileContents ByteString
  deriving (Eq, Read, Show)

data FileData = FileData
  { fileName        :: FilePath
  , fileSize        :: Word32
  , filePermissions :: FileMode
  , fileData        :: FileContents
  } deriving (Eq, Read, Show)

newtype FilePack = FilePack { getPackedFiles :: [FileData] }
  deriving (Eq, Read, Show)

packFiles :: FilePack -> ByteString
packFiles filePack = 
  B64.encode . BC.pack . show $ filePack

unpackFiles :: ByteString -> Either String FilePack
unpackFiles serializedData =
  B64.decode serializedData >>= readEither . BC.unpack

sampleFilePack :: FilePack
sampleFilePack = FilePack $
  [ FileData "stringFile" 0 0 $ StringFileContents "hello String"
  , FileData "textFile" 0 0 $ TextFileContents "hello text"
  , FileData "binaryFile" 0 0 $ ByteStringFileContents "hello Bytestring"
  ]

class Encode a where
  encode :: a -> ByteString

class Decode a where
  decode :: ByteString -> Either String a

instance Encode ByteString where
  encode = id

instance Decode ByteString where
  decode = Right . id

instance Encode Text where
  encode = encodeUtf8

instance Decode Text where
  decode = Right . decodeUtf8

instance Encode String where
  encode = BC.pack

instance Decode String where
  decode = Right . BC.unpack

instance Encode Word32 where
  encode = word32ToByteString

instance Decode Word32 where
  decode = bytestringToWord32

instance Encode Word16 where
  encode = word16ToByteString

instance Decode Word16 where
  decode = bytestringToWord16

word16ToBytes :: Word16 -> (Word8, Word8)
word16ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. (shift word (-8))
  in (a, b)

word16ToByteString :: Word16 -> ByteString
word16ToByteString word =
  let (a,b) = word16ToBytes word
  in BS.pack [a,b]

consWord16 :: Word16 -> ByteString -> ByteString
consWord16 word bytestring =
  let packedWord = word16ToByteString word
  in packedWord <> bytestring

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

word32ToBytes :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. (shift word (-8))
      c = fromIntegral $ 255 .&. (shift word (-16))
      d = fromIntegral $ 255 .&. (shift word (-24))
    in (a, b, c, d)

word32ToByteString :: Word32 -> ByteString
word32ToByteString word =
  let (a,b,c,d) = word32ToBytes word
  in BS.pack [a,b,c,d]

consWord32 :: Word32 -> ByteString -> ByteString
consWord32 word bytestring =
  let packedWord = word32ToByteString word
  in packedWord <> bytestring

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

instance Encode FileMode where
  encode (CMode fMode) = encode fMode

instance Decode FileMode where
  decode = fmap CMode . decode

