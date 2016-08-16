module Main where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty, class Monoid)
import Prelude

data GCounter n = GCounter String (Map String n)

increment :: forall n
           . (Monoid n, Ord n)
          => n
          -> GCounter n
          -> Maybe (GCounter n)
increment amt (GCounter shardId payload)
  | amt < mempty = Nothing
  | otherwise = let sum = amt <> fold (Map.lookup shardId payload)
                 in Just $ GCounter shardId (Map.insert shardId sum payload)

value :: forall n. (Monoid n) => GCounter n -> n
value (GCounter _ payload) = fold payload
