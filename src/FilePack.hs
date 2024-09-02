{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module FilePack where


import Data.Word (Word32)
import System.Posix.Types (FileMode)
import FilePack.Encoding
import FilePack.Decoding

data FileData a = FileData
  { fileName        :: FilePath
  , fileSize        :: Word32
  , filePermissions :: FileMode
  , fileData        :: a
  } deriving (Eq, Show)

newtype FilePack = FilePack [Packable]

data Packable = forall a. Encode a => 
  Packable { getPackable :: FileData a }

instance Encode a => Encode (FileData a) where
  encode FileData{..} =
    let
      encodedFileName = encodeWithSize fileName
      encodedFileSize = encodeWithSize fileSize
      encodedFilePermissions = encodeWithSize filePermissions
      encodedFileData = encodeWithSize fileData
      encodedData =
        encodedFileName
        <> encodedFileSize
        <> encodedFilePermissions
        <> encodedFileData
      in encode encodedData

instance Encode Packable where
  encode (Packable p) = encode p

instance Encode FilePack where
  encode (FilePack p) = encode p