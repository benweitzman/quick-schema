{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Json.Schema where

import Data.Aeson.Types (Parser)
import Data.Aeson hiding (Object, Number)
import qualified Data.Aeson as A

import Data.Hashable

import qualified Data.HashMap.Strict as HM

import Data.Proxy

import Data.Text hiding (stripSuffix)

import Data.Vector hiding (sequence)

class ToSchema a where
    toSchema :: Proxy a -> Schema

data Field = Plain Schema
           | Optional Schema
           | Exact Value Schema deriving (Eq, Show)

data Schema = Boolean
            | Number
            | Text
            | List (Vector Schema)
            | Object (HM.HashMap Text Field) deriving (Eq, Show)


stripSuffix :: Text -> Text
stripSuffix s
    | "=" `isSuffixOf` s = dropEnd 1 s
    | "?" `isSuffixOf` s = dropEnd 1 s
    | otherwise = s

parseField :: Text -> Value -> Parser Field
parseField key value
    | "=" `isSuffixOf` key = Exact value <$> parseJSON value
    | "?" `isSuffixOf` key = Optional <$> parseJSON value
    | otherwise = Plain <$> parseJSON value

mapKeys :: (Hashable k1, Hashable k2, Eq k2) => (k1 -> k2) -> HM.HashMap k1 v -> HM.HashMap k2 v
mapKeys f = HM.foldlWithKey' (\m' k v -> HM.insert (f k) v m') HM.empty

instance FromJSON Schema where
    parseJSON (Bool _) = return Boolean

    parseJSON (A.Number _) = return Number

    parseJSON (String _) = return Text

    parseJSON (Array a) = List <$> sequence (parseJSON <$> a)

    parseJSON (A.Object o) = Object <$> sequence (mapKeys stripSuffix $ HM.mapWithKey parseField o)

    parseJSON Null = mempty



