{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Json.Schema where

import Data.Aeson.Types (Parser)
import Data.Aeson hiding (Object, Number, Bool, String, Array)
import qualified Data.Aeson as A

import Data.Hashable

import qualified Data.HashMap.Strict as HM

import Data.Monoid

import Data.Proxy

import Data.Scientific

import Data.Text hiding (stripSuffix)

import Data.Vector hiding (sequence)

class ToSchema a where
    toSchema :: Proxy a -> Schema

data FieldQualifier = Plain
                    | Optional
                    | Exact deriving (Eq, Show)

data Field = Field FieldQualifier Schema deriving (Eq, Show)

data Schema = Bool Bool
            | Number Scientific
            | String Text
            | Array (Vector Schema)
            | Object (HM.HashMap Text Field) deriving (Eq, Show)

stripSuffix :: Text -> Text
stripSuffix s
    | "=" `isSuffixOf` s = dropEnd 1 s
    | "?" `isSuffixOf` s = dropEnd 1 s
    | otherwise = s

addSuffix :: Text -> Field -> Text
addSuffix k (Field Plain _) = k
addSuffix k (Field Optional _) = k <> "?"
addSuffix k (Field Exact _) = k <> "="

parseField :: Text -> Value -> Parser Field
parseField key value
    | "=" `isSuffixOf` key = Field Exact <$> parseJSON value
    | "?" `isSuffixOf` key = Field Optional <$> parseJSON value
    | otherwise = Field Plain <$> parseJSON value

mapKeys :: (Hashable k1, Hashable k2, Eq k2) => (k1 -> k2) -> HM.HashMap k1 v -> HM.HashMap k2 v
mapKeys f = HM.foldlWithKey' (\m' k v -> HM.insert (f k) v m') HM.empty

mapKeysWithValues :: (Hashable k1, Hashable k2, Eq k2) => (k1 -> v -> k2) -> HM.HashMap k1 v -> HM.HashMap k2 v
mapKeysWithValues f = HM.foldlWithKey' (\m' k v -> HM.insert (f k v) v m') HM.empty

instance FromJSON Schema where
    parseJSON (A.Bool x) = return (Bool x)

    parseJSON (A.Number s) = return (Number s)

    parseJSON (A.String t) = return (String t)

    parseJSON (A.Array a) = Array <$> sequence (parseJSON <$> a)

    parseJSON (A.Object o) = Object <$> sequence (mapKeys stripSuffix $ HM.mapWithKey parseField o)

    parseJSON Null = mempty

instance ToJSON Schema where
    toJSON (Bool x) = A.Bool x
    toJSON (Number s) = A.Number s
    toJSON (String t) = A.String t
    toJSON (Array v) = A.Array (toJSON <$> v)
    toJSON (Object o) = A.Object (HM.map (\(Field fq s) -> toJSON s) (mapKeysWithValues addSuffix o))
