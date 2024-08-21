module Main (main) where

import FilePack
import qualified Data.ByteString as BS

main :: IO ()
main = putStrLn "Test suite not yet implemented."

testPackFile :: ByteString
testPackFile =
    packFiles sampleFilePack

testUnpackFile :: Either String FilePack
testUnpackFile = unpackFiles testPackFile

testRoundTrip :: FilePack -> Bool
testRoundTrip pack =
    (Right pack) == (unpackFiles $ packFiles pack)