{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module RoundTripSpec where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Primitive (ByteArray)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8)
import Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Tree (Tree)

serializeProp :: forall a. (Serialize a, Eq a, Show a, Arbitrary a) => Property
serializeProp = property \(x :: a) -> decode' (encode x) === x

props :: [Property]
props =
  [ serializeProp @Int,
    serializeProp @(Maybe Int),
    serializeProp @(Maybe (Either (Maybe Int) (Maybe Int))),
    serializeProp @(Tree Int),
    serializeProp @[Int],
    serializeProp @[Either Int16 Int32],
    serializeProp @[(Int, Int64, Either Word8 Word8)],
    serializeProp @Text,
    serializeProp @ByteString,
    serializeProp @ByteArray,
    serializeProp @ShortByteString,
    serializeProp @[Either Text ByteString],
    serializeProp @Float,
    serializeProp @Double,
    serializeProp @Bool,
    serializeProp @Ordering,
    serializeProp @IntSet,
    serializeProp @(Seq Int),
    serializeProp @(Seq Text),
    serializeProp @(IntMap Int),
    serializeProp @(IntMap Text),
    serializeProp @(Map Int Int),
    serializeProp @(Map Int Text),
    serializeProp @(HashMap Int Int),
    serializeProp @(HashMap Int Text)
  ]

spec :: Spec
spec = do
  for_ props $ prop ""
  for_ $(allProperties) $ uncurry prop
