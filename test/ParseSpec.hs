{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module ParseSpec where

import Data.Json.Schema

import Data.Aeson hiding (Object, Number)

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
    describe "parser" $ do
        it "should parse correctly" $ do
            decode "{\"maybeNum?\":50}" `shouldBe` Just (Object [("maybeNum", Optional Number)])
            decode "{\"exactNum=\":50}" `shouldBe` Just (Object [("exactNum", Exact (toJSON (50 :: Integer)) Number)])
            decode "{\"num\":50}" `shouldBe` Just (Object [("num", Plain Number)])
