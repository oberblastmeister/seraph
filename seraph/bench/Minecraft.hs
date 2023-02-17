{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module Minecraft where

import Control.DeepSeq (NFData)
import Data.Int
import Data.Store qualified as S
import Data.Word
import Flat qualified as F
import GHC.Generics (Generic)
import Generic.Random (GenericArbitrarySingle (..), GenericArbitraryU(..))
import Seraph
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()
import Data.ByteString (ByteString)

type Text = ByteString

data GameType
  = Survival
  | Creative
  | Adventure
  | Spectator
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving Arbitrary via GenericArbitraryU GameType

data Item
  = Item
      Int8
      Word8
      Text
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving (Arbitrary) via GenericArbitrarySingle Item

data Abilities
  = Abilities
      Float
      Float
      Bool
      Bool
      Bool
      Bool
      Bool
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving (Arbitrary) via GenericArbitrarySingle Abilities

data RecipeBook
  = RecipeBook
      [Text]
      [Text]
      Bool
      Bool
      Bool
      Bool
      Bool
      Bool
      Bool
      Bool
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving (Arbitrary) via GenericArbitrarySingle RecipeBook

data Entity
  = Entity
      Text
      (Double, Double, Double)
      (Double, Double, Double)
      (Float, Float)
      Float
      Word16
      Word16
      Bool
      Bool
      Bool
      Int32
      [Int32]
      (Maybe Text)
      Bool
      Bool
      Bool
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving (Arbitrary) via GenericArbitrarySingle Entity

data Player
  = Player
      GameType
      GameType
      Int64
      Text
      Word32
      Item
      (Maybe Text)
      Int64
      Int64
      Int64
      (Maybe Bool)
      Word16
      Float
      Float
      Word32
      Word32
      Float
      Int32
      Int32
      [Item]
      [Item]
      Abilities
      (Maybe (Float, Float, Float))
      (Maybe ([Word32], Entity))
      (Maybe Entity)
      (Maybe Entity)
      Bool
      RecipeBook
  deriving (Show, Eq, Generic, NFData, Serialize, S.Store, F.Flat)
  deriving (Arbitrary) via GenericArbitrarySingle Player

seraphEncode :: Player -> ByteString
seraphEncode = encode

storeEncode :: Player -> ByteString
storeEncode = S.encode

seraphDecode :: ByteString -> Player
seraphDecode = decode'

storeDecode :: ByteString -> Player
storeDecode = fromRight' . S.decode

fromRight' :: Either a b -> b
fromRight' (Left _) = undefined
fromRight' (Right x) = x
