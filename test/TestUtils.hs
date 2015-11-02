{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module TestUtils where

import Data.Json.Schema

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

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

instance Arbitrary Text where
  arbitrary = dropWhileEnd (liftM2 (||) (=='=') (=='?')) . pack <$> listOf1 validChars
    where validChars = chr <$> choose (55, 126)

instance Arbitrary Schema where
  arbitrary = sized sizedSchema

sizedSchema :: Int -> Gen Schema
sizedSchema 0 = oneof [ Bool <$> arbitrary
                      , Number <$> arbitrary
                      , String <$> arbitrary
                      , return $ Array []
                      , return $ Object []
                      ]
sizedSchema n = oneof [ Bool <$> arbitrary
                      , Number <$> arbitrary
                      , String <$> arbitrary
                      , Array . fromList <$> vectorOf (n - 1) (sizedSchema (n `div` 5))
                      , Object . HM.fromList <$> vectorOf (n - 1) ((,) <$> arbitrary <*> sizedField (n `div` 5))
                      ]

sizedField :: Int -> Gen Field
sizedField n = Field <$> arbitrary <*> sizedSchema n

instance Arbitrary FieldQualifier where
  arbitrary = oneof [return Plain, return Optional, return Exact]
