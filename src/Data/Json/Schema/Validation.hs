module Data.Json.Schema.Validation where

import Data.Json.Schema

import Data.Aeson hiding (Object, Number, Array, String, Bool)

import qualified Data.Aeson as A

import Data.Maybe

import qualified Data.HashMap.Strict as HM

import Data.Text hiding (all, any)

isValidFor :: Value -> Schema -> Bool
isValidFor = flip validate

validate :: Schema -> Value -> Bool
validate (Bool _) (A.Bool _) = True
validate (Number _) (A.Number _) = True
validate (String _) (A.String _) = True
validate (Array ss) (A.Array vs) = all (\v -> any (`validate` v) ss) vs
validate (Object ss) (A.Object vs) = HM.foldlWithKey' (\a k s -> a && validateField s vs k) True ss
validate _ _ = False

validateField :: Field -> HM.HashMap Text Value -> Text -> Bool
validateField (Field Plain s) m key = fromMaybe False $ validate s <$> HM.lookup key m
validateField (Field Exact s) m key = fromMaybe False $ (== schemaToJSON s) <$> HM.lookup key m
validateField (Field Optional s) m key = fromMaybe True $ validate s <$> HM.lookup key m

schemaToJSON :: Schema -> Value
schemaToJSON (Bool b) = A.Bool b
schemaToJSON (Number n) = A.Number n
schemaToJSON (String s) = A.String s
schemaToJSON (Array a) = A.Array (schemaToJSON <$> a)
schemaToJSON (Object o) = A.Object $ (\(Field _ s) -> schemaToJSON s) <$> o
