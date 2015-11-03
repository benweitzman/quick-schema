module Main where

import Data.Json.Schema

import Data.Aeson

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy as BS

import Control.Monad.Trans.Maybe

import Options.Applicative

import System.Exit

data Source = StdIn
            | File FilePath
            | Argument ByteString deriving (Eq, Show)

data Options = Options
  { schemaSource :: Source
  , jsonSource :: Source
  } deriving (Eq, Show)

options :: Parser Options
options = Options <$>
          ((Argument <$>
            (pack <$> strOption (long "schema"
                    <> short 's'
                    <> metavar "SCHEMA"
                    <> help "Use the specified SCHEMA as the schema")
            )) <|> (File <$>
                 strOption (long "schemaFile"
                         <> short 'f'
                         <> metavar "SCHEMA_FILE"
                         <> help "Read the schema from SCHEMA_FILE")
           )) <*>
          ((Argument <$>
            (pack <$> strOption (long "json"
                       <> short 'j'
                    <> metavar "JSON"
                    <> help "Use the specified JSON as the json")
            )) <|> (File <$>
                 strOption (long "jsonFile"
                         <> short 'g'
                         <> metavar "JSON_FILE"
                         <> help "Read the json from JSON_FILE")
           ) <|> pure StdIn)

readSource :: Source -> IO ByteString
readSource (Argument x) = return x
readSource (File f) = BS.readFile f
readSource StdIn = BS.getContents


main :: IO ()
main = do
  Options ss js <- execParser (info options mempty)
  result <- runMaybeT $ do
    schema <- MaybeT $ decode <$> readSource ss
    json <- MaybeT $ decode <$> readSource js
    return $ validate schema json
  case result of
    Nothing -> putStrLn "Error parsing input" >> exitFailure
    Just False -> putStrLn "JSON does not match schema" >> exitFailure
    Just True -> putStrLn "JSON matches schema" >> exitSuccess
