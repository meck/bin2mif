{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.Mif
import Options.Applicative

data Input = FileInput FilePath | StdInput

data Output = FileOutput FilePath | StdOutput

data Options = Options
  { input :: Input,
    output :: Output,
    wordWidth :: Int
  }

fileInput :: Parser Input
fileInput =
  FileInput
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "FILENAME"
          <> help "Input file"
      )

fileOutput :: Parser Output
fileOutput =
  FileOutput
    <$> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILENAME"
          <> help "Output file"
      )

optParser :: Parser Options
optParser =
  Options
    <$> (fileInput <|> pure StdInput)
    <*> (fileOutput <|> pure StdOutput)
    <*> option
      auto
      ( long "width"
          <> short 'w'
          <> help "The word width in bits"
          <> showDefault
          <> value 8
          <> metavar "INT"
      )

opts =
  info
    (optParser <**> helper)
    ( fullDesc
        <> progDesc "Convert binary files to .mif files with arbitrary word width"
        <> header "bin2mif"
    )

main :: IO ()
main = do
  op <- execParser opts

  input <- case input op of
    FileInput fp -> BS.readFile fp
    StdInput -> BS.getContents

  let result = makeMif (wordWidth op) input

  case output op of
    FileOutput fp -> BS.writeFile fp result
    StdOutput -> BS.putStr result
