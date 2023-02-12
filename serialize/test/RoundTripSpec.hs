{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module RoundTripSpec where

import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word8)
import Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

serializeProp :: forall a. (Serialize a, Eq a, Show a, Arbitrary a) => Property
serializeProp = property \(x :: a) -> decode' (encode x) === x

props :: [Property]
props =
  [ serializeProp @Int,
    serializeProp @(Maybe Int),
    serializeProp @(Maybe (Either (Maybe Int) (Maybe Int))),
    serializeProp @[Int],
    serializeProp @[Either Int16 Int32],
    serializeProp @[(Int, Int64, Either Word8 Word8)],
    serializeProp @Text,
    serializeProp @ByteString,
    serializeProp @[Either Text ByteString],
    serializeProp @Float,
    serializeProp @Double,
    serializeProp @Bool,
    serializeProp @Ordering,
    serializeProp @IntSet,
    serializeProp @(Seq Text),
    serializeProp @(IntMap Int)
  ]

spec :: Spec
spec = do
  for_ props $ prop ""
  for_ $(allProperties) $ uncurry prop
