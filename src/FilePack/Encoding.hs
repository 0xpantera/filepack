{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module FilePack.Encoding where

import Data.Bits ((.&.), shift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8, Word16, Word32)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.Posix.Types (FileMode, CMode(..))

class Encode a where
  encode :: a -> ByteString
  encode = BS.drop 4 . encodeWithSize

  encodeWithSize :: a -> ByteString
  encodeWithSize a =
    let s = encode a
        l = fromIntegral $ BS.length s
    in word32ToByteString l <> s
  {-# MINIMAL encode | encodeWithSize #-}

instance Encode ByteString where
  encode = id

instance Encode Text where
  encode = encodeUtf8

instance Encode String where
  encode = BC.pack

instance Encode FileMode where
  encode (CMode fMode) = encode fMode
  encodeWithSize (CMode fMode) = encodeWithSize fMode

instance Encode Word32 where
  encode = word32ToByteString
  encodeWithSize w = 
    let (a,b,c,d) = word32ToBytes w
    in BS.pack [ 4, 0, 0, 0
               , a, b, c, d ]

instance Encode Word16 where
  encode = word16ToByteString
  encodeWithSize w =
    let (a,b) = word16ToBytes w
    in BS.pack [ 2, 0
               , a, b ]

instance (Encode a, Encode b) => Encode (a,b) where
  encode (a,b) = 
    encode $ encodeWithSize a <> encodeWithSize b

-- OVERLAPPABLE because of conflict with String instance
instance {-# OVERLAPPABLE #-} Encode a => Encode [a] where
  encode = encode . foldMap encodeWithSize

word16ToBytes :: Word16 -> (Word8, Word8)
word16ToBytes word =
  let a = fromIntegral $ 255 .&. word
      b = fromIntegral $ 255 .&. (shift word (-8))
  in (a, b)

word16ToByteString :: Word16 -> ByteString
word16ToByteString word =
  let (a,b) = word16ToBytes word
  in BS.pack [a,b]

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