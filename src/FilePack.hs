{-# LANGUAGE OverloadedStrings #-}
module FilePack where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as B64
import Data.Word
import Data.Text (Text)
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