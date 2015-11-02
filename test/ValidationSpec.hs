{-# LANGUAGE ScopedTypeVariables #-}

module ValidationSpec where

import Data.Json.Schema
import Data.Json.Schema.Validation
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
    describe "validation" $ do
        prop "any json should be it's own validation" $
          \(s::Schema) -> validate s (schemaToJSON s)
