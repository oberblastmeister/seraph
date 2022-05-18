{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Serialize.Internal where

import Data.Proxy

data Buffer

data Put a

data Get a

class Serialize a where
  constSize :: Maybe Int
  size :: a -> Int
  serialize :: a -> Put ()

class Deserialize a where
  deserialize :: Get a
