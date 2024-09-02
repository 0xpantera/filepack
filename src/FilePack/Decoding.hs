{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module FilePack.Decoding where

import Data.Bits ((.|.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
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
  decode = fmap CMode . decode

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