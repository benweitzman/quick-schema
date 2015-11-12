{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseSpec where

import Data.Json.Schema
import TestUtils

import Control.Monad

import Data.Aeson hiding (Object, Number, Bool, Array, String)

import Data.Char (chr)

import qualified Data.HashMap.Strict as HM

import Data.Scientific

import Data.Text

import Data.Vector

import System.IO.Unsafe

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck

spec :: Spec
spec =
    describe "parser" $ do
        it "should parse correctly" $ do
            decode "{\"maybeNum?\":50}" `shouldBe` Just (Object [("maybeNum", Field Optional (Number 50))])
            decode "{\"exactNum=\":50}" `shouldBe` Just (Object [("exactNum", Field Exact (Number 50))])
            decode "{\"num\":50}" `shouldBe` Just (Object [("num", Field Plain (Number 50))])
            decode "{\"flatten/\":\"hello\"}" `shouldBe` (Nothing :: Maybe Schema)
            decode "{\"flatten/\":[\"hello\", \"world\"]}" `shouldBe` Just (Object [("flatten", Field Flat (Array [String "hello", String "world"]))])
        prop "should make the round trip" $
            \(s::Schema) -> decode (encode s) == Just s
