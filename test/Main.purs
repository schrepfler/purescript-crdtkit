module Test.Main where

import Prelude
import Data.Maybe (Maybe(Just))
import Data.Map as Map
import Data.Monoid.Additive (Additive(Additive))
import Main (GCounter(..), increment, value)

import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main = runTest do
  suite "GCounter test suite" do
    test "construct GCounter with value 0" do
      let c0 = Just (GCounter "a" (Map.singleton "a" (Additive 0)))
      Assert.assert "Counter should be 0" $ (value <$> c0) == Just (Additive 0)
    test "construct GCounter with value 10" do
      let c10 = Just (GCounter "a" (Map.singleton "a" (Additive 10)))
      Assert.assert "Counter should be 10" $ (value <$> c10) == Just (Additive 10)
    test "construct GCounter with value -10" do
      Assert.assert "Counter must throw exception if constructed with negative number" $ true
    test "construct GCounter with value 0 and increment by 10" do
      let c0 = Just (GCounter "a" (Map.singleton "a" (Additive 0)))
            >>= increment (Additive 10)
      Assert.assert "Counter should be 10" $ (value <$> c0) == Just (Additive 10)
    test "construct GCounter with value 0 and increment by 10 and increment by 1" do
      let c0 = Just (GCounter "a" (Map.singleton "a" (Additive 0)))
            >>= increment (Additive 10)
            >>= increment (Additive 1)
      Assert.assert "Counter should be 11" $ (value <$> c0) == Just (Additive 11)
    test "construct GCounter a with 0 and increment by 3, construct GCounter b with 10 and increment by 5, merge a b" do
      Assert.assert "Counter c should be 18" $ false
