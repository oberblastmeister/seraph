{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module BinTree where

import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Store qualified as S
import Data.Typeable (Typeable)
import Dataset
import Flat qualified as F
import GHC.Generics (Generic)
import Serialize
import Test.QuickCheck

data BinTree a
  = Tree
      (BinTree a)
      (BinTree a)
  | Leaf a
  deriving (Show, Read, Eq, Typeable, Generic, NFData)

data Direction
  = North
  | South
  | Center
  | East
  | West
  deriving (Show, Read, Eq, Typeable, Generic, NFData)

instance Arbitrary Direction where
  arbitrary = elements [North, South, Center, East, West]

-- General instances
instance F.Flat a => F.Flat (BinTree a)

instance F.Flat Direction

instance S.Store a => S.Store (BinTree a)

instance S.Store Direction

instance Serialize a => Serialize (BinTree a)

instance Serialize Direction

storeSize :: S.Size (BinTree Direction)
storeSize = S.size

serializeSize :: BinTree Direction -> Int
serializeSize = size

serializeDecode :: Get (BinTree Direction)
serializeDecode = get

storeDecode :: S.Peek (BinTree Direction)
storeDecode = S.peek
