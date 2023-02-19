{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RoundTripSpec where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Foldable (for_)
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Primitive (ByteArray)
import Data.Primitive qualified as Primitive
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tree (Tree)
import Data.Vector qualified as VB
import Data.Vector.Generic qualified as GVector
import Data.Vector.Primitive qualified as VP
import Data.Word (Word64, Word8)
import Seraph
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

serializeProp :: forall a. (Serialize a, Eq a, Show a, Arbitrary a) => Property
serializeProp = property \(x :: a) -> decode' (encode x) === x

instance Arbitrary a => Arbitrary (Primitive.Array a) where
  arbitrary = Primitive.fromList <$> arbitrary

props :: [Property]
props =
  [ serializeProp @Int,
    serializeProp @(Maybe Int),
    serializeProp @(Maybe (Either (Maybe Int) (Maybe Int))),
    serializeProp @(Tree Int),
    serializeProp @[Int],
    serializeProp @[Either Int16 Int32],
    serializeProp @(NonEmpty Int),
    serializeProp @(NonEmpty ByteString),
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
    serializeProp @(Set Int),
    serializeProp @(Set Text),
    serializeProp @(HashMap Int Int),
    serializeProp @(HashMap Int Text),
    serializeProp @(VB.Vector Int),
    serializeProp @(VB.Vector ByteString),
    serializeProp @(Primitive.Array Int),
    serializeProp @(Primitive.Array ByteString)
  ]

spec :: Spec
spec = do
  for_ props $ prop ""
  for_ $(allProperties) $ uncurry prop

instance (Primitive.Prim a, Arbitrary a) => Arbitrary (VP.Vector a) where
  arbitrary = arbitraryVector
  shrink = shrinkVector

arbitraryVector :: (GVector.Vector v a, Arbitrary a) => Gen (v a)
arbitraryVector = GVector.fromList `fmap` arbitrary

shrinkVector :: (GVector.Vector v a, Arbitrary a) => v a -> [v a]
shrinkVector = fmap GVector.fromList . shrink . GVector.toList
