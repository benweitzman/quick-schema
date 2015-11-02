{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module ValidationSpec where

import Data.Json.Schema
import Data.Json.Schema.Validation
import TestUtils

import Control.Monad

import Data.Aeson hiding (Object, Number, Bool, Array, String)
import qualified Data.Aeson as A

import Data.Char (chr)

import qualified Data.HashMap.Strict as HM

import Data.Scientific

import Data.Text

import Data.Vector

import System.IO.Unsafe

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

taggedUnion :: Schema
taggedUnion = Array [ Object [("type", Field Exact $ String "person")
                             ,("data", Field Plain $ Object [("age", Field Plain $ Number 24)
                                                            ,("name", Field Plain $ String "Ben")
                                                            ,("favoriteColor", Field Optional $ String "Purple")
                                                            ])
                             ]
                    , Object [("type", Field Exact $ String "Address")
                             ,("data", Field Plain $ Object [("street", Field Plain $ String "Prospect Street")
                                                            ,("number", Field Plain $ Number 54)
                                                            ,("city", Field Plain $ String "Cambridge")
                                                            ])
                             ]
                    ]

spec :: Spec
spec =
    describe "validation" $ do
        prop "any json should be it's own validation" $
          \(s::Schema) -> validate s (schemaToJSON s)
        describe "simple types" $ do
          it "should validate bools" $
            property $ \(b1, b2) -> A.Bool b1 `isValidFor` Bool b2
          it "should validate numbers" $
            property $ \(n1, n2) -> A.Number n1 `isValidFor` Number n2
          it "should validate strings" $
            property $ \(s1, s2) -> A.String s1 `isValidFor` String s2
          it "should not validate different types" $ do
            (A.Bool True `isValidFor` Number 3) `shouldBe` False
            (A.Number 44 `isValidFor` Bool False) `shouldBe` False
            (A.Bool False `isValidFor` String "hello") `shouldBe` False
            (A.String "howdy" `isValidFor` Bool True) `shouldBe` False
        describe "objects" $ do
          it "should require required fields" $
            (A.Object [] `isValidFor` Object [("required", Field Plain $ String "text")]) `shouldBe` False
          it "should accept required fields" $
            (A.Object [("required", A.String "some text")] `isValidFor` Object [("required", Field Plain $ String "text")]) `shouldBe` True
          it "should not require optional fields" $
            (A.Object [] `isValidFor` Object [("optional", Field Optional $ String "text")]) `shouldBe` True
          it "should accept optional fields " $
            (A.Object [("optional", A.String "some text")] `isValidFor` Object [("optional", Field Optional $ String "text")]) `shouldBe` True
          it "should not accept optional fields with the wrong type" $
            (A.Object [("optional", A.Number 42)] `isValidFor` Object [("optional", Field Optional $ String "text")]) `shouldBe` False
          it "should not accept exact fields with different data" $
            (A.Object [("exact", A.String "some text")] `isValidFor` Object [("exact", Field Exact $ String "text")]) `shouldBe` False
          it "should handle multiple fields" $ do
            (A.Object [("required", A.String "some text"), ("optional", A.Number 42)] `isValidFor` Object [("required", Field Plain $ String "text"), ("optional", Field Optional $ Number 44)]) `shouldBe` True
            (A.Object [("required", A.String "some text")] `isValidFor` Object [("required", Field Plain $ String "text"), ("optional", Field Optional $ Number 44)]) `shouldBe` True
            (A.Object [("optional", A.Number 42)] `isValidFor` Object [("required", Field Plain $ String "text"), ("optional", Field Optional $ Number 44)]) `shouldBe` False
          it "should handle nested objects" $ do
            (A.Object [("outer", A.Object [("inner", A.String "data")])] `isValidFor` Object [("outer", Field Plain $ Object [("inner", Field Plain $ String "something")])]) `shouldBe` True
            (A.Object [("type", A.String "person"), ("data", A.Object [("age", A.Number 50), ("name", A.String "Albert")])] `isValidFor` Object [("type", Field Exact $ String "person")
                                                                                                                                                ,("data", Field Plain $ Object [("age", Field Plain $ Number 50)
                                                                                                                                                                               ,("name", Field Plain $ String "Some guy")
                                                                                                                                                                               ])
                                                                                                                                                ]) `shouldBe` True
            (A.Object [("type", A.String "persoe"), ("data", A.Object [("age", A.Number 50), ("name", A.String "Albert")])] `isValidFor` Object [("type", Field Exact $ String "person")
                                                                                                                                                ,("data", Field Plain $ Object [("age", Field Plain $ Number 50)
                                                                                                                                                                               ,("name", Field Plain $ String "Some guy")
                                                                                                                                                                               ])
                                                                                                                                                ]) `shouldBe` False
            (A.Object [("type", A.String "person"), ("data", A.Object [("name", A.String "Albert")])] `isValidFor` Object [("type", Field Exact $ String "person")
                                                                                                                                                ,("data", Field Plain $ Object [("age", Field Plain $ Number 50)
                                                                                                                                                                               ,("name", Field Plain $ String "Some guy")
                                                                                                                                                                               ])
                                                                                                                                                ]) `shouldBe` False
        describe "arrays" $ do
          it "should always accept empty arrays" $
            property $ \s -> A.Array [] `isValidFor` Array [s]
          it "should make sure every element matches at least one schema" $ do
            (A.Array [A.Number 42, A.String "Hello World!"] `isValidFor` Array [Number 34, String "Goodbye!"]) `shouldBe` True
            (A.Array [A.String "Hello World!"] `isValidFor` Array [Number 34, String "Goodbye!"]) `shouldBe` True
            (A.Array [A.Number 42, A.String "Hello World!"] `isValidFor` Array [Number 34]) `shouldBe` False
        describe "tagged unions" $ do
          it "Should be able to encode tagged unions" $ do
            (A.Array [A.Object [("type", A.String "person")]] `isValidFor` taggedUnion) `shouldBe` False
            (A.Array [A.Object [("type", A.String "person")]] `isValidFor` Array [Object [("type", Field Exact $ String "person")]]) `shouldBe` True
            (A.Array [A.Object [("type", A.String "persor")]] `isValidFor` Array [Object [("type", Field Exact $ String "person")]]) `shouldBe` False
            (A.Array [A.Object [("type", A.String "person"), ("data", A.Object [("age", A.Number 50), ("name", A.String "Albert")])]] `isValidFor` taggedUnion) `shouldBe` True
            (A.Array [A.Object [("type", A.String "persons"), ("data", A.Object [("age", A.Number 50), ("name", A.String "Albert")])]] `isValidFor` taggedUnion) `shouldBe` False
