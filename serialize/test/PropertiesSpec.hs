{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module PropertiesSpec where

import Data.Foldable (for_)
import Data.Int
import Data.Word (Word8)
import Serialize
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Data.Text (Text)
import Data.ByteString (ByteString)

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
    serializeProp @[Either Text ByteString]
  ]

spec :: Spec
spec = do
  for_ props $ prop ""
  -- it "smoke" $ do
  --   encode @[Int] [] `shouldBe` "0"
  --   pure @IO ()
  for_ $(allProperties) $ uncurry prop
