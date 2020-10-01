{-# LANGUAGE OverloadedStrings #-}

module Data.Mif (makeMif) where

import Data.Binary.Strict.BitGet as BG
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import Numeric (showHex)

makeMif :: Int -> ByteString -> ByteString
makeMif width input = B.concat $ [header width (length datasec)] <> datasec <> [footer]
  where
    datasec = formatChunk <$> zip [0 ..] (chunkBS width input)

header :: Int -> Int -> ByteString
header width depth =
  BS8.pack $
    unlines
      [ "DEPTH = " <> show depth <> ";",
        "WIDTH = " <> show width <> ";",
        "ADDRESS_RADIX = HEX;",
        "DATA_RADIX = BIN;",
        "",
        "CONTENT;",
        "BEGIN;"
      ]

footer :: ByteString
footer = "END;"

formatChunk :: (Int, ByteString) -> ByteString
formatChunk (addr, value) = B.concat [(BS8.pack $ showHex addr ""), " : ", value, ";\n"]

chunkBS :: Int -> ByteString -> [ByteString]
chunkBS width input = do
  let foo = BG.runBitGet input $ parseWords width
  case foo of
    Left err -> error err
    Right res -> res

parseWords :: Int -> BitGet [ByteString]
parseWords width = do
  empty <- BG.isEmpty
  if empty
    then return []
    else do
      w <- parseWord width
      ws <- parseWords width
      pure $ w : ws

-- parse a word with width
parseWord :: Int -> BitGet ByteString
parseWord width = getBitsAsByteString width <$> BG.getRightByteString width

-- Gets n number of bits from a bytestring as text
getBitsAsByteString :: Int -> ByteString -> ByteString
getBitsAsByteString width x = B.concat $ (bitsToBS . toBits) <$> B.unpack x
  where
    bitsToBS bits = B.concat $ (\c -> if c then "1" else "0") <$> bits
    toBits w = [testBit w i | i <- [width -1, width -2 .. 0]]
