{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module FilePack where


import Data.ByteString (ByteString)
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

instance Decode a => Decode (FileData a) where
  decode = execParser $ FileData
    <$> extractValue
    <*> extractValue
    <*> extractValue
    <*> extractValue

testRoundTrip :: (Encode a, Decode a, Show a, Eq a) => a -> IO ()
testRoundTrip val = case decode (encode val) of
  Left err -> putStrLn $ "Failed to round-trip value: " <> err
  Right val'
    | val' == val -> putStrLn "It works!"
    | otherwise -> do
      putStrLn "Round-trip failed!"
      putStrLn $ "expected: " <> show val
      putStrLn $ "got:      " <> show val'

runRoundTripTest :: IO ()
runRoundTripTest =
  testRoundTrip $ FileData
  { fileName = "c"
  , fileSize = 8
  , filePermissions = 0644
  , fileData = (0, "zero") :: (Word32, String)
  }